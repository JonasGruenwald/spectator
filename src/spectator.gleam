import gleam/bool
import gleam/bytes_tree
import gleam/erlang/application
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
import gleam/otp/supervision
import gleam/result
import gleam/string
import gleam/uri
import logging
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

fn start_server(port: Int) -> supervision.ChildSpecification(sup.Supervisor) {
  // Start mist server
  let empty_body = mist.Bytes(bytes_tree.new())
  let not_found = response.set_body(response.new(404), empty_body)

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
        let assert Ok(priv) = application.priv_directory("spectator")
        let path = priv <> "/lucy_spectator.svg"
        mist.send_file(path, offset: 0, limit: option.None)
        |> result.map(fn(favicon) {
          response.new(200)
          |> response.prepend_header("content-type", "image/svg+xml")
          |> response.set_body(favicon)
        })
        |> result.lazy_unwrap(fn() {
          response.new(404)
          |> response.set_body(mist.Bytes(bytes_tree.new()))
        })
      }
      ["connect-widget.js"] -> {
        let assert Ok(priv) = application.priv_directory("spectator")
        let path = priv <> "/connect-widget.js"
        mist.send_file(path, offset: 0, limit: option.None)
        |> result.map(fn(script) {
          response.new(200)
          |> response.prepend_header("content-type", "application/javascript")
          |> response.set_body(script)
        })
        |> result.lazy_unwrap(fn() {
          response.new(404)
          |> response.set_body(mist.Bytes(bytes_tree.new()))
        })
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
      <> atom.to_string(node.self() |> node.name())
    io.println(message)
  })
  |> mist.port(port)
  |> mist.supervised()
}

/// Start the spectator application on port 3000
pub fn start() {
  start_on(3000)
}

pub fn start_on(
  port: Int,
) -> Result(actor.Started(sup.Supervisor), actor.StartError) {
  sup.new(sup.OneForOne)
  |> sup.add(
    supervision.worker(fn() {
      case api.start_tag_manager() {
        Ok(pid) -> Ok(actor.Started(pid, Nil))
        Error(error) ->
          Error(actor.InitFailed(
            "Failed to start tag manager: " <> string.inspect(error),
          ))
      }
    }),
  )
  |> sup.add(start_server(port))
  |> sup.start()
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
  let assert Ok(pid) = process.subject_owner(sub)
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
    Error(_) -> {
      let self = node.self() |> node.name() |> atom.to_string()
      Ok(self)
    }
    Ok(node) -> {
      let self = node.self() |> node.name()
      use <- bool.guard(
        self == atom.create("nonode@nohost"),
        Error(NotDistributedError),
      )

      let node_atom = atom.create(node)
      let cookie_validation_passed = case common.get_param(params, "cookie") {
        // No cookie, validation passes
        Error(_) -> True
        Ok(cookie) -> {
          let cookie_atom = atom.create(cookie)
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

      Ok(atom.to_string(node_atom))
    }
  }
}

fn connect_widget_script() {
  html.script([attribute.src("/connect-widget.js")], "")
}

fn render_server_component(
  title: String,
  server_component_path path: String,
  params params: common.Params,
) {
  let res = response.new(200)
  let styles = common.static_file("styles.css")
  let html = case validate_node_connection(params) {
    Ok(connection_name) -> {
      html([], [
        html.head([], [
          html.title([], title),
          server_component.script(),
          html.meta([attribute.attribute("charset", "utf-8")]),
          html.link([
            attribute.rel("icon"),
            attribute.href("/favicon.svg"),
            attribute.type_("image/svg+xml"),
          ]),
          html.style([], styles),
        ]),
        html.body([], [
          navbar.render(
            title,
            connection_name,
            common.sanitize_params(params)
              |> common.encode_params(),
          ),
          connect_widget_script(),
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
          html.title([], title <> " - Connection Failed"),
          html.meta([attribute.attribute("charset", "utf-8")]),
          html.link([
            attribute.rel("icon"),
            attribute.href("/favicon.svg"),
            attribute.type_("image/svg+xml"),
          ]),
          html.style([], styles),
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
              html.button([attribute.class("change-target-button")], [
                html.text("Change Inspection Target"),
              ]),
            ]),
          ]),
          connect_widget_script(),
        ]),
      ])
    }
  }
  response.set_body(
    res,
    html
      |> element.to_document_string
      |> bytes_tree.from_string
      |> mist.Bytes,
  )
}

//  SERVER COMPONENT WIRING ----------------------------------------------------

type Socket(a) {
  Socket(
    component: lustre.Runtime(a),
    self: process.Subject(server_component.ClientMessage(a)),
  )
}

fn connect_server_component(
  req: Request(Connection),
  lustre_application,
  params: common.Params,
) {
  let socket_init = fn(_conn: WebsocketConnection) {
    let self = process.new_subject()
    let app = lustre_application()

    let assert Ok(component) = lustre.start_server_component(app, params)

    tag_subject(
      server_component.subject(component),
      "__spectator_internal Server Component",
    )

    server_component.register_subject(self)
    |> lustre.send(to: component)

    let selector = process.new_selector() |> process.select(self)
    #(Socket(component:, self:), option.Some(selector))
  }

  let socket_update = fn(state: Socket(a), msg, conn) {
    case msg {
      mist.Text(json) -> {
        // we attempt to decode the incoming text as an action to send to our
        // server component runtime.
        let action =
          json.parse(json, server_component.runtime_message_decoder())

        case action {
          Ok(action) -> lustre.send(state.component, action)
          Error(_) -> Nil
        }

        mist.continue(state)
      }

      mist.Binary(_) -> mist.continue(state)
      mist.Custom(patch) -> {
        let assert Ok(_) =
          patch
          |> server_component.client_message_to_json()
          |> json.to_string
          |> mist.send_text_frame(conn, _)

        mist.continue(state)
      }
      mist.Closed | mist.Shutdown -> mist.stop()
    }
  }

  let socket_close = fn(state: Socket(a)) {
    server_component.deregister_subject(state.self)
    |> lustre.send(to: state.component)

    lustre.shutdown()
    |> lustre.send(to: state.component)
  }

  mist.websocket(
    request: req,
    on_init: socket_init,
    handler: socket_update,
    on_close: socket_close,
  )
}
