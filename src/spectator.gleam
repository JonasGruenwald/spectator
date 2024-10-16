import gleam/bytes_builder
import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/io
import lustre/element
import lustre/element/html.{html}
import mist.{type Connection, type ResponseData}
import spectator/internal/api
import spectator/internal/views/process_table

/// Start the spectator application.
pub fn main() {
  let empty_body = mist.Bytes(bytes_builder.new())
  let not_found = response.set_body(response.new(404), empty_body)

  let assert Ok(_) =
    fn(req: Request(Connection)) -> Response(ResponseData) {
      case request.path_segments(req) {
        ["processes", "static"] -> show_process_list()
        ["greet", name] -> greet(name)
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
pub fn tag(name: String) -> Nil {
  api.add_tag(name)
}

fn show_process_list() -> Response(ResponseData) {
  let res = response.new(200)
  let assert Ok(info) = api.get_info_list()
  let html =
    html([], [
      html.head([], [html.title([], "Greetings!")]),
      html.body([], [process_table.render(info)]),
    ])
  response.set_body(
    res,
    html
      |> element.to_document_string
      |> bytes_builder.from_string
      |> mist.Bytes,
  )
}

fn greet(name: String) -> Response(ResponseData) {
  let res = response.new(200)
  let html =
    html([], [
      html.head([], [html.title([], "Greetings!")]),
      html.body([], [html.h1([], [html.text("Hey there, " <> name <> "!")])]),
    ])

  response.set_body(
    res,
    html
      |> element.to_document_string
      |> bytes_builder.from_string
      |> mist.Bytes,
  )
}
