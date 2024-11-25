import gleam/bool
import gleam/bytes_builder
import gleam/dynamic
import gleam/erlang
import gleam/erlang/atom
import gleam/erlang/node
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
import gleam/uri
import lustre
import lustre/attribute
import lustre/element
import lustre/element/html.{html}
import lustre/server_component
import mist.{type Connection, type ResponseData, type WebsocketConnection}
import spectator/internal/api
import spectator/internal/common
import spectator/internal/components/dashboard_live
import spectator/internal/components/ets_overview_live
import spectator/internal/components/ets_table_live
import spectator/internal/components/ports_live
import spectator/internal/components/processes_live
import spectator/internal/views/navbar

/// Entrypoint for running spectator from the command line.
/// This will start the spectator application on port 3000 and never return.
pub fn main() {
  let assert Ok(_) = start()
  process.sleep_forever()
}

fn start_server(port: Int) -> Result(process.Pid, Nil) {
  // Start mist server
  let empty_body = mist.Bytes(bytes_builder.new())
  let not_found = response.set_body(response.new(404), empty_body)
  let server_result =
    fn(req: Request(Connection)) -> Response(ResponseData) {
      let query_params = request.get_query(req) |> result.unwrap([])
      case request.path_segments(req) {
        // App Routes
        ["dashboard"] ->
          render_server_component("Dashboard", "dashboard-feed", query_params)
        ["processes"] ->
          render_server_component("Processes", "process-feed", query_params)
        ["ets"] -> render_server_component("ETS", "ets-feed", query_params)
        ["ets", table] ->
          render_server_component("ETS", "ets-feed/" <> table, query_params)
        ["ports"] -> render_server_component("Ports", "port-feed", query_params)
        // WebSocket Routes
        ["dashboard-feed"] ->
          connect_server_component(req, dashboard_live.app, query_params)
        ["process-feed"] ->
          connect_server_component(req, processes_live.app, query_params)
        ["ets-feed"] ->
          connect_server_component(req, ets_overview_live.app, query_params)
        ["ets-feed", table] ->
          connect_server_component(req, ets_table_live.app, [
            #("table_name", uri.percent_decode(table) |> result.unwrap("")),
            ..query_params
          ])
        ["port-feed"] ->
          connect_server_component(req, ports_live.app, query_params)
        // Static files
        ["favicon.svg"] -> {
          return_static("/lucy_spectator.svg", "image/svg+xml")
        }
        ["styles.css"] -> {
          return_static("/styles.css", "text/css")
        }
        ["lustre-server-component.mjs"] -> {
          return_static(
            "/lustre-server-component.mjs",
            "application/javascript",
          )
        }
        // Redirect to dashboard by default
        [] -> {
          response.new(302)
          |> response.prepend_header("location", "/dashboard")
          |> response.set_body(empty_body)
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
        <> " - Node: "
        <> atom.to_string(node.self() |> node.to_atom())
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
    Error(_e) -> {
      Error(Nil)
    }
  }
}

/// Start the spectator application on port 3000
pub fn start() {
  start_on(3000)
}

pub fn start_on(port: Int) -> Result(process.Pid, dynamic.Dynamic) {
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

type NodeConnectionError {
  NotDistributedError
  FailedToSetCookieError
  FailedToConnectError
}

fn validate_node_connection(
  params: common.Params,
) -> Result(String, NodeConnectionError) {
  let node_res = common.get_param(params, "node")
  case node_res {
    // No node passed, that's fine, we'll just use the local node
    // no other checks are needed
    Error(_) -> Ok("")
    Ok(node) -> {
      let self = node.self() |> node.to_atom()
      use <- bool.guard(
        self == atom.create_from_string("nonode@nohost"),
        Error(NotDistributedError),
      )

      let node_atom = atom.create_from_string(node)
      let cookie_validation_passed = case common.get_param(params, "cookie") {
        // No cookie, validation passes
        Error(_) -> True
        Ok(cookie) -> {
          let cookie_atom = atom.create_from_string(cookie)
          result.unwrap(api.set_cookie(node_atom, cookie_atom), False)
        }
      }

      use <- bool.guard(
        !cookie_validation_passed,
        Error(FailedToSetCookieError),
      )

      use <- bool.guard(
        !result.unwrap(api.hidden_connect_node(node_atom), False),
        Error(FailedToConnectError),
      )

      Ok("🟢 " <> atom.to_string(node_atom))
    }
  }
}

//  WEB RESPONSES ----------------------------------------------------------
fn return_static(file_path: String, content_type: String) {
  let assert Ok(priv) = erlang.priv_directory("spectator")
  let path = priv <> file_path
  mist.send_file(path, offset: 0, limit: option.None)
  |> result.map(fn(favicon) {
    response.new(200)
    |> response.prepend_header("content-type", content_type)
    |> response.set_body(favicon)
  })
  |> result.lazy_unwrap(fn() {
    response.new(404)
    |> response.set_body(mist.Bytes(bytes_builder.new()))
  })
}

fn render_server_component(
  title: String,
  server_component_path path: String,
  params params: common.Params,
) {
  let res = response.new(200)
  let html = case validate_node_connection(params) {
    Ok(connection_name) -> {
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
          html.meta([attribute.attribute("charset", "utf-8")]),
          html.link([
            attribute.rel("icon"),
            attribute.href("/favicon.svg"),
            attribute.type_("image/svg+xml"),
          ]),
          html.link([attribute.rel("stylesheet"), attribute.href("/styles.css")]),
        ]),
        html.body([], [
          navbar.render(
            title,
            connection_name,
            common.sanitize_params(params)
              |> common.encode_params(),
          ),
          element.element(
            "lustre-server-component",
            [
              server_component.route(
                "/" <> path <> common.encode_params(params),
              ),
            ],
            [],
          ),
        ]),
      ])
    }
    Error(connection_error) -> {
      html([], [
        html.head([], [
          html.title([], title),
          html.meta([attribute.attribute("charset", "utf-8")]),
          html.link([
            attribute.rel("icon"),
            attribute.href("/favicon.svg"),
            attribute.type_("image/svg+xml"),
          ]),
          html.link([attribute.rel("stylesheet"), attribute.href("/styles.css")]),
        ]),
        html.body([], [
          navbar.render(
            title,
            "Connection Failed",
            common.sanitize_params(params)
              |> common.encode_params(),
          ),
          html.div([attribute.class("component-error")], [
            html.div([], [html.text("Node connection failed:")]),
            html.div([], [
              html.text(case connection_error {
                NotDistributedError ->
                  "Node is not distributed, cannot connect to other nodes. Please start the spectator instance in distributed mode by setting a node name."
                FailedToSetCookieError ->
                  "Failed to set cookie, could not apply the cookie to the node"
                FailedToConnectError ->
                  "Failed to connect to node, please check the node name and cookie"
              }),
            ]),
            html.div([], [
              html.a([attribute.href("/"), attribute.class("button")], [
                html.text("Return to local node"),
              ]),
            ]),
          ]),
        ]),
      ])
    }
  }
  response.set_body(
    res,
    html
      |> element.to_document_string
      |> bytes_builder.from_string
      |> mist.Bytes,
  )
}

//  SERVER COMPONENT WIRING ----------------------------------------------------

fn connect_server_component(
  req: Request(Connection),
  lustre_application,
  params: common.Params,
) {
  let socket_init = fn(_conn: WebsocketConnection) {
    let self = process.new_subject()
    let app = lustre_application()
    let assert Ok(live_component) = lustre.start_actor(app, params)
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
