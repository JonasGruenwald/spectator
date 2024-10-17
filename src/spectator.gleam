import gleam/bytes_builder
import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import lustre/element
import lustre/element/html.{html}
import mist.{type Connection, type ResponseData}
import spectator/internal/api
import spectator/internal/utils
import spectator/internal/views/navbar
import spectator/internal/views/process_table

/// Start the spectator application.
pub fn main() {
  let empty_body = mist.Bytes(bytes_builder.new())
  let not_found = response.set_body(response.new(404), empty_body)
  let assert Ok(_) =
    fn(req: Request(Connection)) -> Response(ResponseData) {
      case request.path_segments(req) {
        ["processes", "static"] -> show_process_list()
        _ -> not_found
      }
    }
    |> mist.new
    |> mist.port(3000)
    |> mist.start_http

  process.sleep_forever()
}

/// Tag the current process with a name that can be used to identify it in the spectator UI.
/// You must call this function from **within** the process you want to tag.
/// A good place to call it, would be in the `init` function of your process.
pub fn tag(name: String) -> Nil {
  api.add_tag(name)
}

fn show_process_list() -> Response(ResponseData) {
  let res = response.new(200)
  let assert Ok(info) = api.get_info_list()
  let sorted = api.sort_info_list(info, api.Memory, api.Descending)
  let styles = utils.static_file("styles.css")
  let html =
    html([], [
      html.head([], [html.title([], "Process List"), html.style([], styles)]),
      html.body([], [navbar.render(), process_table.render(sorted)]),
    ])
  response.set_body(
    res,
    html
      |> element.to_document_string
      |> bytes_builder.from_string
      |> mist.Bytes,
  )
}
