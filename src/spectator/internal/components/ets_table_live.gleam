import gleam/erlang/atom
import gleam/erlang/process
import gleam/int
import gleam/list
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
    table: Option(api.Table),
    table_data: Option(api.TableData),
    sort_column: Option(Int),
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

/// Sometimes we can't get the table info from an atom directly
/// so we fall back to the slower list lookup here
fn get_ets_table_info_from_list(table_name: atom.Atom) {
  use tables <- result.try(api.list_ets_tables())
  list.find(tables, fn(t) { t.name == table_name })
  |> result.replace_error(Nil)
}

fn get_initial_data(table_name_raw: String) -> Result(Model, Nil) {
  use table_name <- result.try(uri.percent_decode(table_name_raw))
  use table_atom <- result.try(
    atom.from_string(table_name) |> result.replace_error(Nil),
  )
  use table <- result.try(
    result.lazy_or(api.get_ets_table_info(table_atom), fn() {
      get_ets_table_info_from_list(table_atom)
    }),
  )
  let table_data = api.get_ets_data(table) |> option.from_result
  Ok(Model(
    subject: None,
    table: Some(table),
    table_data: table_data,
    sort_column: None,
    sort_direction: api.Descending,
  ))
}

fn init(table_name_raw: String) {
  case get_initial_data(table_name_raw) {
    Ok(model) -> #(model, emit_after(common.refresh_interval, Refresh, None))
    Error(_) -> #(Model(None, None, None, None, api.Descending), effect.none())
  }
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  Refresh
  CreatedSubject(process.Subject(Msg))
  HeadingClicked(Option(Int))
}

fn do_refresh(model: Model) -> Model {
  case model.table {
    Some(t) -> {
      case api.get_ets_data(t), model.sort_column {
        Ok(data), Some(sort_column_index) -> {
          // see it, say it,
          let sorted =
            api.sort_table_data(data, sort_column_index, model.sort_direction)

          Model(..model, table_data: Some(sorted))
        }
        Ok(data), None -> Model(..model, table_data: Some(data))
        Error(_), _ -> Model(None, None, None, None, api.Descending)
      }
    }
    _ -> model
  }
}

fn update(model: Model, msg: Msg) {
  case msg {
    Refresh -> #(
      do_refresh(model),
      emit_after(common.refresh_interval, Refresh, None),
    )

    CreatedSubject(subject) -> #(
      Model(..model, subject: Some(subject)),
      effect.none(),
    )
    HeadingClicked(column) -> {
      case column {
        _c if column == model.sort_column -> {
          #(
            Model(
              ..model,
              sort_direction: api.invert_sort_direction(model.sort_direction),
            )
              |> do_refresh,
            effect.none(),
          )
        }
        c -> {
          #(Model(..model, sort_column: c) |> do_refresh, effect.none())
        }
      }
    }
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  case model.table, model.table_data {
    Some(table), Some(data) -> render_table_data(model, table, data)
    _, _ -> render_not_found()
  }
}

fn render_table_data(model: Model, table: api.Table, data: api.TableData) {
  case data.max_length == 0 {
    True ->
      html.div([attribute.class("ets-table-error")], [
        html.text("No data in table "),
        display.atom(table.name),
      ])
    False ->
      html.table([attribute.class("ets-data")], [
        html.thead([], [
          html.tr(
            [],
            list.range(0, data.max_length - 1)
              |> list.map(fn(i) {
                table.heading(
                  int.to_string(i),
                  int.to_string(i),
                  Some(i),
                  model.sort_column,
                  model.sort_direction,
                  HeadingClicked,
                  False,
                )
              }),
          ),
        ]),
        html.tbody(
          [],
          table.map_rows(data.content, fn(row) {
            html.tr(
              [attribute.class("ets-row")],
              list.map(row, fn(cell) { html.td([], [display.inspect(cell)]) }),
            )
          }),
        ),
      ])
  }
}

fn render_not_found() {
  html.div([attribute.class("ets-table-error")], [
    html.text("The referenced table could not be loaded."),
  ])
}