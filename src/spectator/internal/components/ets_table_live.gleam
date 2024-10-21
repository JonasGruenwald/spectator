import gleam/erlang/atom
import gleam/erlang/process
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
  Ok(Model(subject: None, table: Some(table), table_data: table_data))
}

fn init(table_name_raw: String) {
  case get_initial_data(table_name_raw) {
    Ok(model) -> #(model, emit_after(common.refresh_interval, Refresh, None))
    Error(_) -> #(Model(None, None, None), effect.none())
  }
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  Refresh
  CreatedSubject(process.Subject(Msg))
}

fn update(model: Model, msg: Msg) {
  case msg {
    Refresh ->
      case model.table {
        Some(t) -> {
          #(
            Model(..model, table_data: option.from_result(api.get_ets_data(t))),
            emit_after(common.refresh_interval, Refresh, model.subject),
          )
        }
        _ -> #(model, effect.none())
      }
    CreatedSubject(subject) -> #(
      Model(..model, subject: Some(subject)),
      effect.none(),
    )
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  case model.table, model.table_data {
    Some(table), Some(data) -> render_table_data(model, table, data)
    _, _ -> render_not_found()
  }
}

fn render_table_data(_model: Model, table: api.Table, data: api.TableData) {
  case data.max_length == 0 {
    True ->
      html.div([attribute.class("ets-table-error")], [
        html.text("No data in table "),
        display.atom(table.name),
      ])
    False ->
      html.table([attribute.class("ets-data")], [
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

fn render_not_found() {
  html.div([attribute.class("ets-table-error")], [
    html.text("The referenced table could not be loaded."),
  ])
}
