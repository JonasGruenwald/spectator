import gleam/dynamic
import gleam/erlang/atom
import gleam/erlang/process
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import logging
import lustre
import lustre/attribute
import lustre/effect
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import lustre/server_component
import pprint
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
    process_list: List(api.ProcessItem),
    sort_criteria: api.InfoSortCriteria,
    sort_direction: api.SortDirection,
    active_process: Option(api.ProcessItem),
    details: Option(api.ProcessDetails),
    status: Option(api.Status),
    state: Option(dynamic.Dynamic),
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

fn request_otp_details(
  pid: process.Pid,
  subject: Option(process.Subject(Msg)),
) -> effect.Effect(Msg) {
  case subject {
    Some(sub) -> {
      use _ <- effect.from
      api.request_otp_data(pid, fn(details) {
        process.send(sub, ReceivedOtpDetails(details))
      })
      Nil
    }
    None -> effect.none()
  }
}

fn init(_) -> #(Model, effect.Effect(Msg)) {
  let info = api.get_info_list()
  let default_sort_criteria = api.Reductions
  let default_sort_direction = api.Descending
  let sorted = api.sort_info_list(info, default_sort_criteria, api.Descending)
  #(
    Model(
      subject: option.None,
      process_list: sorted,
      sort_criteria: default_sort_criteria,
      sort_direction: default_sort_direction,
      active_process: option.None,
      details: option.None,
      status: option.None,
      state: option.None,
    ),
    emit_after(common.refresh_interval, Refresh, option.None),
  )
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  Refresh
  ReceivedOtpDetails(details: api.OtpDetails)
  CreatedSubject(process.Subject(Msg))
  ProcessClicked(api.ProcessItem)
  HeadingClicked(api.InfoSortCriteria)
  OtpStateClicked(api.ProcessItem, api.SysState)
  PidClicked(process.Pid)
}

fn do_refresh(model: Model) -> Model {
  let info = api.get_info_list()
  let sorted =
    api.sort_info_list(info, model.sort_criteria, model.sort_direction)

  let active_process = case model.active_process {
    None -> None
    Some(active_process) -> {
      case api.get_info(active_process.pid) {
        Ok(info) -> Some(api.ProcessItem(active_process.pid, info))
        Error(_) -> None
      }
    }
  }

  let details = case active_process {
    None -> None
    Some(ap) -> {
      case api.get_details(ap.pid) {
        Ok(details) -> {
          Some(details)
        }
        Error(_) -> None
      }
    }
  }
  Model(..model, active_process:, process_list: sorted, details:)
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    Refresh -> {
      let new_model = do_refresh(model)
      case new_model.active_process {
        Some(p) if p.info.message_queue_len < common.message_queue_threshold -> #(
          new_model,
          effect.batch([
            request_otp_details(p.pid, model.subject),
            emit_after(common.refresh_interval, Refresh, option.None),
          ]),
        )
        _ -> #(
          new_model,
          emit_after(common.refresh_interval, Refresh, option.None),
        )
      }
    }
    CreatedSubject(subject) -> #(
      Model(..model, subject: Some(subject)),
      effect.none(),
    )
    ProcessClicked(p) -> {
      let new_model =
        Model(..model, active_process: Some(p), state: None, status: None)
        |> do_refresh
      #(new_model, request_otp_details(p.pid, model.subject))
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
    ReceivedOtpDetails(details) -> {
      case model.active_process {
        Some(p) if p.pid == details.pid -> {
          #(
            Model(
              ..model,
              state: Some(details.state),
              status: Some(details.status),
            ),
            effect.none(),
          )
        }
        _ -> #(model, effect.none())
      }
    }
    OtpStateClicked(p, target_sys_state) -> {
      case target_sys_state {
        api.ProcessSuspended -> {
          api.resume(p.pid)
          #(do_refresh(model), request_otp_details(p.pid, model.subject))
        }
        api.ProcessRunning -> {
          api.suspend(p.pid)
          #(do_refresh(model), request_otp_details(p.pid, model.subject))
        }
      }
    }
    PidClicked(pid) -> {
      case api.get_info(pid) {
        Ok(info) -> {
          let p = api.ProcessItem(pid, info)
          let new_model =
            Model(..model, active_process: Some(p), state: None, status: None)
            |> do_refresh
          #(new_model, request_otp_details(p.pid, model.subject))
        }
        Error(_) -> #(model, effect.none())
      }
    }
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  render(
    model.process_list,
    ProcessClicked,
    model.active_process,
    model.details,
    model.status,
    model.state,
    model.sort_criteria,
    model.sort_direction,
    HeadingClicked,
    OtpStateClicked,
  )
}

fn render_name(process: api.ProcessItem) {
  case process.info.registered_name {
    option.None -> display.pid(process.pid)
    option.Some(name) -> html.text(atom.to_string(name))
  }
}

fn render_tag(process: api.ProcessItem) {
  case process.info.tag {
    option.None -> display.function(process.info.initial_call)
    option.Some("__spectator_internal " <> rest) -> html.text("🔍 " <> rest)
    option.Some(tag) -> html.text("🔖 " <> tag)
  }
}

fn render_primitive_list(
  primitives: List(api.SystemPrimitive),
  on_primitive_click: fn(process.Pid) -> Msg,
) {
  list.map(primitives, display.system_primitive_interactive(
    _,
    on_primitive_click,
  ))
  |> list.intersperse(html.text(", "))
}

fn render_details(
  p: Option(api.ProcessItem),
  d: Option(api.ProcessDetails),
  status: Option(api.Status),
  state: Option(dynamic.Dynamic),
  handle_otp_state_click: fn(api.ProcessItem, api.SysState) -> Msg,
) {
  case p, d {
    Some(proc), Some(details) ->
      html.div([attribute.class("details")], [
        html.div([attribute.class("general")], [
          html.div([attribute.class("panel-heading")], [
            case proc.info.tag {
              Some("__spectator_internal " <> rest) ->
                html.text("🔍 Spectator " <> rest)
              Some(tag) -> html.text("🔖 " <> tag <> " details")
              None -> html.text("🎛️ Process Details")
            },
          ]),
          html.div([attribute.class("panel-content")], [
            html.dl([], [
              html.dt([], [html.text("Process Id")]),
              html.dd([], [display.pid(proc.pid)]),
            ]),
            case proc.info.tag {
              option.None -> html.text("")
              option.Some(tag) ->
                html.dl([], [
                  html.dt([], [html.text("Tag")]),
                  html.dd([], [html.text(tag)]),
                ])
            },
            case proc.info.registered_name {
              option.None -> html.text("")
              option.Some(name) ->
                html.dl([], [
                  html.dt([], [html.text("Registered Name")]),
                  html.dd([], [html.text(atom.to_string(name))]),
                ])
            },
            html.dl([], [
              html.dt([], [html.text("Status")]),
              html.dd([], [html.text(atom.to_string(proc.info.status))]),
            ]),
            html.dl([], [
              html.dt([], [html.text("Links")]),
              html.dd([], render_primitive_list(details.links, PidClicked)),
            ]),
            html.dl([], [
              html.dt([], [html.text("Monitored By")]),
              html.dd(
                [],
                render_primitive_list(details.monitored_by, PidClicked),
              ),
            ]),
            html.dl([], [
              html.dt([], [html.text("Monitors")]),
              html.dd([], render_primitive_list(details.monitors, PidClicked)),
            ]),
            html.dl([], [
              html.dt([], [html.text("Parent")]),
              html.dd([], [
                case details.parent {
                  option.None -> html.text("None")
                  option.Some(parent) ->
                    display.system_primitive_interactive(parent, PidClicked)
                },
              ]),
            ]),
            html.dl([], [
              html.dt([], [html.text("Trap Exit")]),
              html.dd([], [display.bool(details.trap_exit)]),
            ]),
            html.dl([], [
              html.dt([], [html.text("Initial Call")]),
              html.dd([], [display.function(proc.info.initial_call)]),
            ]),
            html.dl([], [
              html.dt([], [html.text("Current Function")]),
              html.dd([], [display.function(proc.info.current_function)]),
            ]),
            html.dl([], [
              html.dt([], [html.text("Reductions")]),
              html.dd([], [display.number(proc.info.reductions)]),
            ]),
            html.dl([], [
              html.dt([], [html.text("Memory")]),
              html.dd([], [display.storage_detailed(proc.info.memory)]),
            ]),
            html.dl([], [
              html.dt([], [html.text("Message Queue Length")]),
              html.dd([], [display.number(proc.info.message_queue_len)]),
            ]),
          ]),
        ]),
        html.div([attribute.class("otp")], case status, state {
          Some(status), Some(state) -> {
            [
              html.div([attribute.class("panel-heading")], [
                html.strong([], [html.text("☎️ OTP Process: ")]),
                display.atom(status.module),
                case status.sys_state {
                  api.ProcessSuspended -> {
                    html.button(
                      [
                        event.on_click(handle_otp_state_click(
                          proc,
                          status.sys_state,
                        )),
                        attribute.class("otp-toggle resume"),
                      ],
                      [html.text("🏃‍♀️‍➡️ Resume")],
                    )
                  }
                  api.ProcessRunning -> {
                    html.button(
                      [
                        event.on_click(handle_otp_state_click(
                          proc,
                          status.sys_state,
                        )),
                        attribute.class("otp-toggle suspend"),
                      ],
                      [html.text("✋ Suspend")],
                    )
                  }
                },
              ]),
              html.div([attribute.class("panel-content")], [
                html.pre([], [html.text(pprint.format(state))]),
              ]),
            ]
          }
          _, _ if proc.info.message_queue_len >= common.message_queue_threshold -> [
            html.div([attribute.class("panel-content")], [
              html.strong([], [
                html.text(
                  "⚠️ Spectator has stopped trying to inspect the OTP state of this process",
                ),
              ]),
              html.p([], [
                html.text("The message queue length is above the threshold of "),
                display.number(common.message_queue_threshold),
                html.text("."),
                html.br([]),
                html.text(
                  "Spectator will not send it any more system messages to avoid filling the message queue.",
                ),
              ]),
            ]),
          ]
          _, _ -> [
            html.div([attribute.class("panel-content")], [
              html.text("This process does not appear to be OTP-compliant"),
            ]),
          ]
        }),
      ])
    _, _ ->
      html.div([attribute.class("otp-placeholder")], [
        html.text("Click a process to see details"),
      ])
  }
}

fn classify_selected(process: api.ProcessItem, active: Option(api.ProcessItem)) {
  let selection_status = case active {
    Some(active) if active.pid == process.pid -> "selected"
    _ -> ""
  }
  case process.info.tag {
    option.None -> attribute.class(selection_status)
    option.Some("__spectator_internal " <> _rest) ->
      attribute.class(selection_status <> " spectator-tagged")
    option.Some(_) -> attribute.class(selection_status <> " tagged")
  }
}

pub fn render(
  processes: List(api.ProcessItem),
  handle_process_click: fn(api.ProcessItem) -> Msg,
  active: Option(api.ProcessItem),
  details: Option(api.ProcessDetails),
  status: Option(api.Status),
  state: Option(dynamic.Dynamic),
  sort_criteria: api.InfoSortCriteria,
  sort_direction: api.SortDirection,
  handle_heading_click: fn(api.InfoSortCriteria) -> Msg,
  handle_otp_state_click: fn(api.ProcessItem, api.SysState) -> Msg,
) {
  html.table([], [
    html.thead([], [
      html.tr([], [
        table.heading(
          "Name",
          "Process PID or registered name",
          api.Name,
          sort_criteria,
          sort_direction,
          handle_heading_click,
          align_right: False,
        ),
        table.heading(
          "Tag",
          "Spectator tag or initial call",
          api.Tag,
          sort_criteria,
          sort_direction,
          handle_heading_click,
          align_right: False,
        ),
        table.heading(
          "Current",
          "Current function",
          api.CurrentFunction,
          sort_criteria,
          sort_direction,
          handle_heading_click,
          align_right: False,
        ),
        table.heading(
          "Reductions",
          "Number of reductions",
          api.Reductions,
          sort_criteria,
          sort_direction,
          handle_heading_click,
          align_right: False,
        ),
        table.heading(
          "Memory",
          "Memory usage",
          api.Memory,
          sort_criteria,
          sort_direction,
          handle_heading_click,
          align_right: True,
        ),
        table.heading(
          "Msgs",
          "Message queue size",
          api.MessageQueue,
          sort_criteria,
          sort_direction,
          handle_heading_click,
          align_right: True,
        ),
        table.heading(
          "Status",
          "Process Status",
          api.ProcessStatus,
          sort_criteria,
          sort_direction,
          handle_heading_click,
          align_right: False,
        ),
      ]),
    ]),
    html.tbody(
      [],
      table.map_rows(processes, fn(process) {
        html.tr(
          [
            attribute.role("button"),
            classify_selected(process, active),
            event.on_click(handle_process_click(process)),
          ],
          [
            html.td([], [render_name(process)]),
            html.td([], [render_tag(process)]),
            html.td([], [display.function(process.info.current_function)]),
            html.td([], [display.number(process.info.reductions)]),
            html.td([attribute.class("cell-right")], [
              display.storage(process.info.memory),
            ]),
            html.td([attribute.class("cell-right")], [
              display.number(process.info.message_queue_len),
            ]),
            html.td([], [display.atom(process.info.status)]),
          ],
        )
      }),
    ),
    html.tfoot([], [
      html.tr([], [
        html.td([attribute.attribute("colspan", "7")], [
          render_details(active, details, status, state, handle_otp_state_click),
        ]),
      ]),
    ]),
  ])
}
