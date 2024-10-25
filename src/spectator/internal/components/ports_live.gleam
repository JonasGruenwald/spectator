import gleam/erlang/atom
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import lustre
import lustre/attribute
import lustre/effect
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
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
    subject: Option(process.Subject(Msg)),
    port_list: List(api.PortItem),
    sort_criteria: api.PortSortCriteria,
    sort_direction: api.SortDirection,
    active_port: Option(api.PortItem),
    details: Option(api.PortDetails),
  )
}

fn init(params: common.Params) -> #(Model, effect.Effect(Msg)) {
  let info = api.get_port_list()
  let default_sort_criteria = api.SortByPortInput
  let default_sort_direction = api.Descending
  let sorted = api.sort_port_list(info, default_sort_criteria, api.Descending)
  let active_port = case common.get_param(params, "selected") {
    Error(_) -> None
    Ok(raw_port_id) -> {
      use port_id <- option.then(
        api.decode_port(raw_port_id) |> option.from_result,
      )
      use info <- option.then(api.get_port_info(port_id) |> option.from_result)
      Some(api.PortItem(port_id:, info:))
    }
  }
  let details = case active_port {
    None -> None
    Some(ap) -> {
      case api.get_port_details(ap.port_id) {
        Ok(details) -> Some(details)
        Error(_) -> None
      }
    }
  }
  #(
    Model(
      subject: option.None,
      port_list: sorted,
      sort_criteria: default_sort_criteria,
      sort_direction: default_sort_direction,
      active_port:,
      details:,
    ),
    common.emit_after(
      common.refresh_interval,
      Refresh,
      option.None,
      CreatedSubject,
    ),
  )
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  Refresh
  CreatedSubject(process.Subject(Msg))
  PortClicked(api.PortItem)
  HeadingClicked(api.PortSortCriteria)
}

fn do_refresh(model: Model) -> Model {
  let info = api.get_port_list()
  let sorted =
    api.sort_port_list(info, model.sort_criteria, model.sort_direction)

  let active_port = case model.active_port {
    None -> None
    Some(active_port) -> {
      case api.get_port_info(active_port.port_id) {
        Ok(info) -> Some(api.PortItem(active_port.port_id, info))
        Error(_) -> None
      }
    }
  }

  let details = case active_port {
    None -> None
    Some(ap) -> {
      case api.get_port_details(ap.port_id) {
        Ok(details) -> {
          Some(details)
        }
        Error(_) -> None
      }
    }
  }
  Model(..model, active_port:, port_list: sorted, details:)
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    Refresh -> {
      #(
        do_refresh(model),
        common.emit_after(
          common.refresh_interval,
          Refresh,
          model.subject,
          CreatedSubject,
        ),
      )
    }
    CreatedSubject(subject) -> #(
      Model(..model, subject: Some(subject)),
      effect.none(),
    )
    PortClicked(p) -> {
      let new_model =
        Model(..model, active_port: Some(p))
        |> do_refresh
      #(new_model, effect.none())
    }
    HeadingClicked(criteria) -> {
      case criteria {
        c if c == model.sort_criteria -> {
          let new_model =
            Model(
              ..model,
              sort_direction: api.invert_sort_direction(model.sort_direction),
            )
          #(do_refresh(new_model), effect.none())
        }
        c -> {
          let new_model = Model(..model, sort_criteria: c)
          #(do_refresh(new_model), effect.none())
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
          "Port ID or registered name",
          api.SortByPortName,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          align_right: False,
        ),
        table.heading(
          "Command",
          "Command name that started the port",
          api.SortByPortCommand,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          align_right: False,
        ),
        table.heading(
          "Process",
          "Process connected to the port",
          api.SortByPortConnectedProcess,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          align_right: False,
        ),
        table.heading(
          "OS PID",
          "Operating system process ID",
          api.SortByPortOsPid,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          align_right: False,
        ),
        table.heading(
          "Input",
          "Input: Number of bytes read from the port",
          api.SortByPortInput,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          align_right: True,
        ),
        table.heading(
          "Output",
          "Output: Number of bytes written to the port",
          api.SortByPortOutput,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          align_right: True,
        ),
        table.heading(
          "Memory",
          "Memory allocated for this port by the runtime system",
          api.SortByPortMemory,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          align_right: True,
        ),
        table.heading(
          "Queue",
          "Queue Size",
          api.SortByPortQueueSize,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          align_right: True,
        ),
      ]),
    ]),
    html.tbody(
      [],
      table.map_rows(model.port_list, fn(port) {
        html.tr(
          [
            attribute.role("button"),
            case model.active_port {
              Some(active) if active.port_id == port.port_id ->
                attribute.class("selected")
              _ -> attribute.none()
            },
            event.on_click(PortClicked(port)),
          ],
          [
            html.td([], [render_name(port)]),
            html.td([], [html.text(port.info.command_name)]),
            html.td([attribute.class("link-cell")], [
              display.system_primitive(port.info.connected_process),
            ]),
            html.td([], [
              case port.info.os_pid {
                Some(pid) -> display.number(pid)
                None -> html.text("N/A")
              },
            ]),
            html.td([attribute.class("cell-right")], [
              display.storage(port.info.input),
            ]),
            html.td([attribute.class("cell-right")], [
              display.storage(port.info.output),
            ]),
            html.td([attribute.class("cell-right")], [
              display.storage(port.info.memory),
            ]),
            html.td([attribute.class("cell-right")], [
              display.number(port.info.queue_size),
            ]),
          ],
        )
      }),
    ),
    html.tfoot([], [
      html.tr([], [
        html.td([attribute.attribute("colspan", "8")], [
          render_details(model.active_port, model.details),
        ]),
      ]),
    ]),
  ])
}

fn render_name(process: api.PortItem) {
  case process.info.registered_name {
    option.None -> display.port(process.port_id)
    option.Some(name) -> html.text(atom.to_string(name))
  }
}

fn render_primitive_list(primitives: List(api.SystemPrimitive)) {
  list.map(primitives, display.system_primitive)
  |> list.intersperse(html.text(", "))
}

fn render_details(p: Option(api.PortItem), d: Option(api.PortDetails)) {
  case p, d {
    Some(port), Some(details) ->
      html.div([attribute.class("details compact")], [
        html.dl([], [
          html.dt([], [html.text("Connected Process")]),
          html.dd([], [display.system_primitive(port.info.connected_process)]),
        ]),
        html.dl([], [
          html.dt([], [html.text("Links")]),
          html.dd([], render_primitive_list(details.links)),
        ]),
        html.dl([], [
          html.dt([], [html.text("Monitored By")]),
          html.dd([], render_primitive_list(details.monitored_by)),
        ]),
        html.dl([], [
          html.dt([], [html.text("Monitors")]),
          html.dd([], render_primitive_list(details.monitors)),
        ]),
      ])
    _, _ ->
      html.div([attribute.class("footer-placeholder")], [
        html.text("Click a process to see details"),
      ])
  }
}
