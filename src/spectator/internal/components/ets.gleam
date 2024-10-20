import gleam/list
import gleam/result
import lustre
import lustre/attribute
import lustre/effect
import lustre/element.{type Element}
import lustre/element/html
import spectator/internal/api
import spectator/internal/views/display
import spectator/internal/views/table

// MAIN ------------------------------------------------------------------------

pub fn app() {
  lustre.application(init, update, view)
}

// MODEL -----------------------------------------------------------------------

pub type Model {
  Model(
    tables: List(api.Table),
    sort_criteria: api.TableSortCriteria,
    sort_direction: api.SortDirection,
  )
}

fn init(_) {
  let defaul_sort_criteria = api.TableName
  let defaul_sort_direction = api.Ascending
  #(
    Model(
      tables: get_sorted_tables(defaul_sort_criteria, defaul_sort_direction),
      sort_criteria: defaul_sort_criteria,
      sort_direction: defaul_sort_direction,
    ),
    effect.none(),
  )
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  Refresh
  HeadingClicked(api.TableSortCriteria)
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
        tables: result.unwrap(api.list_ets_tables(), [])
          |> api.sort_table_list(model.sort_criteria, model.sort_direction),
      ),
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
        // table.heading(
        //   "ID",
        //   "Table Identifier",
        //   api.TableId,
        //   model.sort_criteria,
        //   model.sort_direction,
        //   HeadingClicked,
        //   False,
        // ),
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
      list.map(model.tables, fn(t) {
        html.tr([], [
          // html.td([], [display.reference(t.id)]),
          html.td([], [display.atom(t.name)]),
          html.td([], [display.atom(t.table_type)]),
          html.td([attribute.class("cell-right")], [
            display.number(t.size),
            html.text(" items"),
          ]),
          html.td([attribute.class("cell-right")], [display.storage(t.memory)]),
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
  ])
}
