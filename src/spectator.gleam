import gleam/bytes_builder
import gleam/erlang
import gleam/erlang/process
import gleam/http
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/io
import gleam/json
import gleam/option
import gleam/otp/actor
import gleam/otp/static_supervisor as sup
import gleam/result
import lustre
import lustre/attribute
import lustre/element
import lustre/element/html.{html}
import lustre/server_component
import mist.{type Connection, type ResponseData, type WebsocketConnection}
import spectator/internal/api
import spectator/internal/components/processes_live
import spectator/internal/utils
import spectator/internal/views/navbar

fn start_server(port: Int) -> Result(process.Pid, Nil) {
  // Start mist server
  let empty_body = mist.Bytes(bytes_builder.new())
  let not_found = response.set_body(response.new(404), empty_body)
  let server_result =
    fn(req: Request(Connection)) -> Response(ResponseData) {
      case request.path_segments(req) {
        ["process-feed"] -> connect_server_component(req, processes_live.app)
        [] -> {
          response.new(302)
          |> response.prepend_header("location", "/processes")
          |> response.set_body(empty_body)
        }
        ["processes"] -> render_server_component("Processes", "process-feed")
        // Serve static server component
        ["lustre-server-component.mjs"] -> {
          let assert Ok(priv) = erlang.priv_directory("lustre")
          let path = priv <> "/static/lustre-server-component.mjs"
          mist.send_file(path, offset: 0, limit: option.None)
          |> result.map(fn(script) {
            response.new(200)
            |> response.prepend_header("content-type", "application/javascript")
            |> response.set_body(script)
          })
          |> result.lazy_unwrap(fn() {
            response.new(404)
            |> response.set_body(mist.Bytes(bytes_builder.new()))
          })
        }
        ["favicon.svg"] -> {
          let assert Ok(priv) = erlang.priv_directory("spectator")
          let path = priv <> "/lucy_spectator.svg"
          mist.send_file(path, offset: 0, limit: option.None)
          |> result.map(fn(favicon) {
            response.new(200)
            |> response.prepend_header("content-type", "image/svg+xml")
            |> response.set_body(favicon)
          })
          |> result.lazy_unwrap(fn() {
            response.new(404)
            |> response.set_body(mist.Bytes(bytes_builder.new()))
          })
        }

        _ -> not_found
      }
    }
    |> mist.new
    |> mist.after_start(fn(port, scheme, interface) {
      let address = case interface {
        mist.IpV6(..) -> "[" <> mist.ip_address_to_string(interface) <> "]"
        _ -> mist.ip_address_to_string(interface)
      }
      let message =
        "🔍 Spectator is listening on "
        <> http.scheme_to_string(scheme)
        <> "://"
        <> address
        <> ":"
        <> int.to_string(port)
      io.println(message)
    })
    |> mist.port(port)
    |> mist.start_http

  // Extract PID for supervisor
  case server_result {
    Ok(server) -> {
      let server_pid = process.subject_owner(server)
      tag(server_pid, "__spectator_internal Server")
      Ok(server_pid)
    }
    Error(e) -> {
      io.debug(#("Failed to start spectator mist server ", e))
      Error(Nil)
    }
  }
}

/// Start the spectator application on port 3000
pub fn start() {
  start_on(3000)
}

pub fn start_on(port: Int) {
  sup.new(sup.OneForOne)
  |> sup.add(sup.worker_child("Spectator Tag Manager", api.start_tag_manager))
  |> sup.add(
    sup.worker_child("Spectator Mist Server", fn() { start_server(port) }),
  )
  |> sup.start_link()
}

/// Tag a process given by PID with a name for easier identification in the spectator UI.
/// You must call `start` before calling this function.
pub fn tag(pid: process.Pid, name: String) -> process.Pid {
  api.add_tag(pid, name)
  pid
}

/// Tag a process given by subject with a name for easier identification in the spectator UI.
/// You must call `start` before calling this function.
pub fn tag_subject(
  subject sub: process.Subject(a),
  name name: String,
) -> process.Subject(a) {
  let pid = process.subject_owner(sub)
  tag(pid, name)
  sub
}

/// Tag a process given by subject result with a name for easier identification in the spectator UI.
/// You must call `start` before calling this function.
pub fn tag_result(
  result: Result(process.Subject(a), b),
  name: String,
) -> Result(process.Subject(a), b) {
  case result {
    Ok(sub) -> Ok(tag_subject(sub, name))
    other -> other
  }
}

fn render_server_component(title: String, server_component_path path: String) {
  let res = response.new(200)
  let styles = utils.static_file("styles.css")
  let html =
    html([], [
      html.head([], [
        html.title([], title),
        html.script(
          [
            attribute.type_("module"),
            attribute.src("/lustre-server-component.mjs"),
          ],
          "",
        ),
        html.link([
          attribute.rel("icon"),
          attribute.href("/favicon.svg"),
          attribute.type_("image/svg+xml"),
        ]),
        html.style([], styles),
      ]),
      html.body([], [
        navbar.render(title),
        element.element(
          "lustre-server-component",
          [server_component.route("/" <> path)],
          [],
        ),
      ]),
    ])
  response.set_body(
    res,
    html
      |> element.to_document_string
      |> bytes_builder.from_string
      |> mist.Bytes,
  )
}

//  SERVER COMPONENT WIRING ----------------------------------------------------

fn connect_server_component(req: Request(Connection), lustre_application) {
  let socket_init = fn(_conn: WebsocketConnection) {
    let self = process.new_subject()
    let app = lustre_application()
    let assert Ok(live_component) = lustre.start_actor(app, 0)

    tag_subject(live_component, "__spectator_internal Server Component")

    process.send(
      live_component,
      server_component.subscribe(
        // server components can have many connected clients, so we need a way to
        // identify this client.
        "ws",
        process.send(self, _),
      ),
    )

    #(
      // we store the server component's `Subject` as this socket's state so we
      // can shut it down when the socket is closed.
      live_component,
      option.Some(process.selecting(process.new_selector(), self, fn(a) { a })),
    )
  }

  let socket_update = fn(live_component, conn: WebsocketConnection, msg) {
    case msg {
      mist.Text(json) -> {
        // we attempt to decode the incoming text as an action to send to our
        // server component runtime.
        let action = json.decode(json, server_component.decode_action)

        case action {
          Ok(action) -> process.send(live_component, action)
          Error(_) -> Nil
        }

        actor.continue(live_component)
      }

      mist.Binary(_) -> actor.continue(live_component)
      mist.Custom(patch) -> {
        let assert Ok(_) =
          patch
          |> server_component.encode_patch
          |> json.to_string
          |> mist.send_text_frame(conn, _)

        actor.continue(live_component)
      }
      mist.Closed | mist.Shutdown -> actor.Stop(process.Normal)
    }
  }

  let socket_close = fn(live_component) {
    process.send(live_component, lustre.shutdown())
  }

  mist.websocket(
    request: req,
    on_init: socket_init,
    on_close: socket_close,
    handler: socket_update,
  )
}
