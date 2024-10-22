import gleam/erlang/atom
import gleam/erlang/process
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/uri
import lustre
import lustre/attribute
import lustre/effect
import lustre/element.{type Element}
import lustre/element/html
import lustre/server_component
import spectator/internal/api
import spectator/internal/common
import spectator/internal/views/display
import spectator/internal/views/table

// MAIN ------------------------------------------------------------------------

pub fn app() {
  lustre.application(init, update, view)
}

// MODEL -----------------------------------------------------------------------

pub type Model {
  Model(
    // Relevant for table list
    subject: Option(process.Subject(Msg)),
    tables: List(api.Table),
    sort_criteria: api.TableSortCriteria,
    sort_direction: api.SortDirection,
  )
}

fn emit_after(
  delay: Int,
  msg: Msg,
  subject: Option(process.Subject(Msg)),
) -> effect.Effect(Msg) {
  case subject {
    Some(self) -> {
      use _ <- effect.from
      let _ = process.send_after(self, delay, msg)
      Nil
    }
    None -> {
      use dispatch, subject <- server_component.select
      let selector =
        process.new_selector() |> process.selecting(subject, fn(msg) { msg })
      let _ = process.send_after(subject, delay, msg)
      dispatch(CreatedSubject(subject))
      selector
    }
  }
}

fn init(_) {
  let defaul_sort_criteria = api.SortByTableSize
  let defaul_sort_direction = api.Descending
  #(
    Model(
      subject: None,
      tables: get_sorted_tables(defaul_sort_criteria, defaul_sort_direction),
      sort_criteria: defaul_sort_criteria,
      sort_direction: defaul_sort_direction,
    ),
    emit_after(common.refresh_interval, Refresh, None),
  )
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  Refresh
  HeadingClicked(api.TableSortCriteria)
  // TableClicked(api.Table)
  CreatedSubject(process.Subject(Msg))
}

fn get_sorted_tables(sort_criteria, sort_direction) -> List(api.Table) {
  result.unwrap(api.list_ets_tables(), [])
  |> api.sort_table_list(sort_criteria, sort_direction)
}

fn update(model: Model, msg: Msg) {
  case msg {
    Refresh -> #(
      Model(
        ..model,
        tables: get_sorted_tables(model.sort_criteria, model.sort_direction),
      ),
      emit_after(common.refresh_interval, Refresh, model.subject),
    )
    CreatedSubject(subject) -> #(
      Model(..model, subject: Some(subject)),
      effect.none(),
    )
    HeadingClicked(criteria) -> {
      case criteria {
        c if c == model.sort_criteria -> {
          let new_direction = api.invert_sort_direction(model.sort_direction)
          let new_model =
            Model(
              ..model,
              tables: get_sorted_tables(c, new_direction),
              sort_direction: new_direction,
            )
          #(new_model, effect.none())
        }
        c -> {
          let new_model =
            Model(
              ..model,
              tables: get_sorted_tables(c, model.sort_direction),
              sort_criteria: c,
            )
          #(new_model, effect.none())
        }
      }
    }
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  html.table([], [
    html.thead([], [
      html.tr([], [
        table.heading(
          "Name",
          "Table Name",
          api.SortByTableName,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          False,
        ),
        table.heading(
          "Type",
          "Table Type",
          api.SortByTableType,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          False,
        ),
        table.heading(
          "Size",
          "Table Size",
          api.SortByTableSize,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          True,
        ),
        table.heading(
          "Memory",
          "Table Memory",
          api.SortByTableMemory,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          True,
        ),
        table.heading(
          "Owner",
          "Table Owner",
          api.SortByTableOwner,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          True,
        ),
        table.heading(
          "Protection",
          "Table Protection",
          api.SortByTableProtection,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          True,
        ),
        table.heading(
          "Read Conc.",
          "Table Read Concurrency",
          api.SortByTableReadConcurrency,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          True,
        ),
        table.heading(
          "Write Conc.",
          "Table Write Concurrency",
          api.SortByTableWriteConcurrency,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          True,
        ),
      ]),
    ]),
    html.tbody(
      [],
      table.map_rows(model.tables, fn(t) {
        html.tr([], [
          html.td([], link_cell(t, [display.atom(t.name)])),
          html.td([], link_cell(t, [display.atom(t.table_type)])),
          html.td(
            [attribute.class("cell-right")],
            link_cell(t, [display.number(t.size), html.text(" items")]),
          ),
          html.td(
            [attribute.class("cell-right")],
            link_cell(t, [display.storage_words(t.memory)]),
          ),
          html.td([attribute.class("cell-right")], [
            display.system_primitive(t.owner),
          ]),
          html.td(
            [attribute.class("cell-right")],
            link_cell(t, [display.atom(t.protection)]),
          ),
          html.td(
            [attribute.class("cell-right")],
            link_cell(t, [display.bool(t.read_concurrency)]),
          ),
          html.td(
            [attribute.class("cell-right")],
            link_cell(t, [display.bool(t.write_concurrency)]),
          ),
        ])
      }),
    ),
    html.tfoot([], [
      html.tr([], [
        html.td([attribute.attribute("colspan", "8")], [
          html.div([attribute.class("footer-placeholder")], [
            html.text("Click on a table to view data"),
          ]),
        ]),
      ]),
    ]),
  ])
}

fn link_cell(
  t: api.Table,
  children: List(element.Element(Msg)),
) -> List(element.Element(Msg)) {
  [
    html.a(
      [attribute.href("/ets/" <> uri.percent_encode(atom.to_string(t.name)))],
      children,
    ),
  ]
}
