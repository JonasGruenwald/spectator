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
    node: api.ErlangNode,
    params: common.Params,
    subject: Option(process.Subject(Msg)),
    refresh_interval: Int,
    tables: List(api.Table),
    word_size: Int,
    sort_criteria: api.TableSortCriteria,
    sort_direction: api.SortDirection,
  )
}

fn init(params) {
  let node = api.node_from_params(params)
  let defaul_sort_criteria = api.SortByTableSize
  let defaul_sort_direction = api.Descending
  let refresh_interval = common.get_refresh_interval(params)
  let word_size = result.unwrap(api.get_word_size(node), 8)
  #(
    Model(
      node:,
      params: common.sanitize_params(params),
      subject: None,
      refresh_interval:,
      tables: get_sorted_tables(
        node,
        defaul_sort_criteria,
        defaul_sort_direction,
      ),
      word_size:,
      sort_criteria: defaul_sort_criteria,
      sort_direction: defaul_sort_direction,
    ),
    common.emit_after(refresh_interval, Refresh, None, CreatedSubject),
  )
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  Refresh
  HeadingClicked(api.TableSortCriteria)
  CreatedSubject(process.Subject(Msg))
}

fn get_sorted_tables(
  node: api.ErlangNode,
  sort_criteria,
  sort_direction,
) -> List(api.Table) {
  result.unwrap(api.list_ets_tables(node), [])
  |> api.sort_table_list(sort_criteria, sort_direction)
}

fn update(model: Model, msg: Msg) {
  case msg {
    Refresh -> #(
      Model(
        ..model,
        tables: get_sorted_tables(
          model.node,
          model.sort_criteria,
          model.sort_direction,
        ),
      ),
      common.emit_after(
        model.refresh_interval,
        Refresh,
        model.subject,
        CreatedSubject,
      ),
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
              tables: get_sorted_tables(model.node, c, new_direction),
              sort_direction: new_direction,
            )
          #(new_model, effect.none())
        }
        c -> {
          let new_model =
            Model(
              ..model,
              tables: get_sorted_tables(model.node, c, model.sort_direction),
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
          html.td(
            [attribute.class("link-cell")],
            link_cell(t, [display.atom(t.name)], model.params),
          ),
          html.td(
            [attribute.class("link-cell")],
            link_cell(t, [display.atom(t.table_type)], model.params),
          ),
          html.td(
            [attribute.class("cell-right link-cell")],
            link_cell(
              t,
              [display.number(t.size), html.text(" items")],
              model.params,
            ),
          ),
          html.td(
            [attribute.class("cell-right link-cell")],
            link_cell(
              t,
              [display.storage_words(t.memory, model.word_size)],
              model.params,
            ),
          ),
          html.td([attribute.class("cell-right link-cell")], [
            display.system_primitive(t.owner, model.params),
          ]),
          html.td(
            [attribute.class("cell-right link-cell")],
            link_cell(t, [display.atom(t.protection)], model.params),
          ),
          html.td(
            [attribute.class("cell-right link-cell")],
            link_cell(t, [display.bool(t.read_concurrency)], model.params),
          ),
          html.td(
            [attribute.class("cell-right link-cell")],
            link_cell(t, [display.bool(t.write_concurrency)], model.params),
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
  params: common.Params,
) -> List(element.Element(Msg)) {
  [
    html.a(
      [
        attribute.href(
          "/ets/"
          <> uri.percent_encode(atom.to_string(t.name))
          <> common.encode_params(params),
        ),
      ],
      children,
    ),
  ]
}
