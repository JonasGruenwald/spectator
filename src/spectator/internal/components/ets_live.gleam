import gleam/erlang/atom
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import lustre
import lustre/attribute
import lustre/effect
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
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
    // Relevant for inspected table
    active_table: Option(api.Table),
    active_table_data: Option(api.TableData),
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
  let defaul_sort_criteria = api.Size
  let defaul_sort_direction = api.Descending
  #(
    Model(
      subject: None,
      tables: get_sorted_tables(defaul_sort_criteria, defaul_sort_direction),
      sort_criteria: defaul_sort_criteria,
      sort_direction: defaul_sort_direction,
      active_table: None,
      active_table_data: None,
    ),
    emit_after(common.refresh_interval, Refresh, None),
  )
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  Refresh
  HeadingClicked(api.TableSortCriteria)
  TableClicked(api.Table)
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
    TableClicked(table) -> {
      #(
        Model(
          ..model,
          active_table: Some(table),
          active_table_data: api.get_ets_data(table) |> option.from_result,
        ),
        effect.none(),
      )
    }
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
  case model.active_table, model.active_table_data {
    Some(table), Some(data) -> render_table_data(model, table, data)
    _, _ -> render_table_overview(model)
  }
}

fn render_table_data(_model: Model, table: api.Table, data: api.TableData) {
  case data.max_length == 0 {
    True ->
      html.div([attribute.class("ets-table-empty")], [
        html.text("No data in table "),
        display.atom(table.name),
      ])
    False ->
      html.table([], [
        html.thead([], [html.tr([], render_numbered_headers(data.max_length))]),
        html.tbody(
          [],
          table.map_rows(data.content, fn(row) {
            html.tr(
              [],
              list.map(row, fn(cell) { html.td([], [display.inspect(cell)]) }),
            )
          }),
        ),
      ])
  }
}

fn render_numbered_headers(count: Int) {
  list.range(0, count - 1)
  |> list.map(fn(i) { html.th([], [display.number(i)]) })
}

fn render_table_overview(model: Model) {
  html.table([], [
    html.thead([], [
      html.tr([], [
        table.heading(
          "Name",
          "Table Name",
          api.TableName,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          False,
        ),
        table.heading(
          "Type",
          "Table Type",
          api.TableType,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          False,
        ),
        table.heading(
          "Size",
          "Table Size",
          api.Size,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          True,
        ),
        table.heading(
          "Memory",
          "Table Memory",
          api.TableMemory,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          True,
        ),
        table.heading(
          "Owner",
          "Table Owner",
          api.Owner,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          True,
        ),
        table.heading(
          "Protection",
          "Table Protection",
          api.Protection,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          True,
        ),
        table.heading(
          "Read Conc.",
          "Table Read Concurrency",
          api.ReadConcurrency,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          True,
        ),
        table.heading(
          "Write Conc.",
          "Table Write Concurrency",
          api.WriteConcurrency,
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
        html.tr([event.on_click(TableClicked(t))], [
          html.td([], [display.atom(t.name)]),
          html.td([], [display.atom(t.table_type)]),
          html.td([attribute.class("cell-right")], [
            display.number(t.size),
            html.text(" items"),
          ]),
          html.td([attribute.class("cell-right")], [
            display.storage_words(t.memory),
          ]),
          html.td([attribute.class("cell-right")], [display.pid(t.owner)]),
          html.td([attribute.class("cell-right")], [display.atom(t.protection)]),
          html.td([attribute.class("cell-right")], [
            display.bool(t.read_concurrency),
          ]),
          html.td([attribute.class("cell-right")], [
            display.bool(t.write_concurrency),
          ]),
        ])
      }),
    ),
    html.tfoot([], [
      html.tr([], [
        html.td([attribute.attribute("colspan", "8")], [
          html.div([attribute.class("ets-footer")], [
            display.number(list.length(model.tables)),
            html.text(" tables found in total"),
          ]),
        ]),
      ]),
    ]),
  ])
}
