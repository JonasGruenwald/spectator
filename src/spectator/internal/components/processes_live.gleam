import gleam/dynamic
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
    refresh_interval: Int,
    process_list: List(api.ProcessItem),
    sort_criteria: api.ProcessSortCriteria,
    sort_direction: api.SortDirection,
    active_process: Option(api.ProcessItem),
    details: Option(api.ProcessDetails),
    status: Option(api.ProcessOtpStatus),
    state: Option(dynamic.Dynamic),
  )
}

fn emit_message(msg: Msg) -> effect.Effect(Msg) {
  effect.from(fn(dispatch) { dispatch(msg) })
}

fn request_otp_details(
  pid: process.Pid,
  subject: Option(process.Subject(Msg)),
) -> effect.Effect(Msg) {
  case subject {
    Some(sub) -> {
      use _ <- effect.from
      api.request_otp_data(
        // TODO USE NODE
        None,
        pid,
        fn(details) { process.send(sub, ReceivedOtpDetails(details)) },
      )
      Nil
    }
    None -> effect.none()
  }
}

fn init(params: common.Params) -> #(Model, effect.Effect(Msg)) {
  let info =
    api.get_process_list(
      // TODO USE NODE
      None,
    )
  let default_sort_criteria = api.SortByReductions
  let default_sort_direction = api.Descending
  let refresh_interval = common.get_refresh_interval(params)
  let sorted =
    api.sort_process_list(info, default_sort_criteria, api.Descending)
  #(
    Model(
      subject: option.None,
      refresh_interval:,
      process_list: sorted,
      sort_criteria: default_sort_criteria,
      sort_direction: default_sort_direction,
      active_process: option.None,
      details: option.None,
      status: option.None,
      state: option.None,
    ),
    effect.batch([
      common.emit_after(refresh_interval, Refresh, option.None, CreatedSubject),
      case common.get_param(params, "selected") {
        Error(_) -> effect.none()
        Ok(potential_pid) -> {
          case api.decode_pid(potential_pid) {
            Ok(pid) -> emit_message(PidClicked(pid))
            Error(_) -> effect.none()
          }
        }
      },
    ]),
  )
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  Refresh
  ReceivedOtpDetails(details: api.OtpDetails)
  CreatedSubject(process.Subject(Msg))
  ProcessClicked(api.ProcessItem)
  HeadingClicked(api.ProcessSortCriteria)
  OtpStateClicked(api.ProcessItem, api.SysState)
  PidClicked(process.Pid)
  KillClicked(api.ProcessItem)
}

fn do_refresh(model: Model) -> Model {
  let info =
    api.get_process_list(
      // TODO USE NODE
      None,
    )
  let sorted =
    api.sort_process_list(info, model.sort_criteria, model.sort_direction)

  let active_process = case model.active_process {
    None -> None
    Some(active_process) -> {
      case
        api.get_process_info(
          // TODO USE NODE
          None,
          active_process.pid,
        )
      {
        Ok(info) -> Some(api.ProcessItem(active_process.pid, info))
        Error(_) -> None
      }
    }
  }

  let details = case active_process {
    None -> None
    Some(ap) -> {
      case
        api.get_details(
          // TODO USE NODE
          None,
          ap.pid,
        )
      {
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
            common.emit_after(
              model.refresh_interval,
              Refresh,
              model.subject,
              CreatedSubject,
            ),
          ]),
        )
        _ -> #(
          new_model,
          common.emit_after(
            model.refresh_interval,
            Refresh,
            model.subject,
            CreatedSubject,
          ),
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
          api.resume(
            // TODO USE NODE
            None,
            p.pid,
          )
          #(do_refresh(model), request_otp_details(p.pid, model.subject))
        }
        api.ProcessRunning -> {
          api.suspend(
            // TODO USE NODE
            None,
            p.pid,
          )
          #(do_refresh(model), request_otp_details(p.pid, model.subject))
        }
      }
    }
    PidClicked(pid) -> {
      case
        api.get_process_info(
          // TODO USE NODE
          None,
          pid,
        )
      {
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
    KillClicked(p) -> {
      api.kill_process(
        // TODO USE NODE
        None,
        p.pid,
      )
      #(do_refresh(model), effect.none())
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
          "Process PID or registered name",
          api.SortByProcessName,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          align_right: False,
        ),
        table.heading(
          "Tag",
          "Spectator tag or initial call",
          api.SortByTag,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          align_right: False,
        ),
        table.heading(
          "Current",
          "Current function",
          api.SortByCurrentFunction,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          align_right: False,
        ),
        table.heading(
          "Reductions",
          "Number of reductions",
          api.SortByReductions,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          align_right: False,
        ),
        table.heading(
          "Memory",
          "Memory usage",
          api.SortByProcessMemory,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          align_right: True,
        ),
        table.heading(
          "Msgs",
          "Message queue size",
          api.SortByMessageQueue,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          align_right: True,
        ),
        table.heading(
          "Status",
          "Process Status",
          api.SortByProcessStatus,
          model.sort_criteria,
          model.sort_direction,
          HeadingClicked,
          align_right: True,
        ),
      ]),
    ]),
    html.tbody(
      [],
      table.map_rows(model.process_list, fn(process) {
        html.tr(
          [
            attribute.role("button"),
            classify_selected(process, model.active_process),
            event.on_click(ProcessClicked(process)),
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
            html.td([attribute.class("cell-right")], [
              display.atom(process.info.status),
            ]),
          ],
        )
      }),
    ),
    html.tfoot([], [
      html.tr([], [
        html.td([attribute.attribute("colspan", "7")], [
          render_details(
            model.active_process,
            model.details,
            model.status,
            model.state,
            OtpStateClicked,
          ),
        ]),
      ]),
    ]),
  ])
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
  status: Option(api.ProcessOtpStatus),
  state: Option(dynamic.Dynamic),
  handle_otp_state_click: fn(api.ProcessItem, api.SysState) -> Msg,
) {
  case p, d {
    Some(proc), Some(details) ->
      html.div([attribute.class("details")], [
        html.div([attribute.class("general")], [
          html.div([attribute.class("panel-heading")], [
            case proc.info.tag, proc.info.registered_name {
              Some("__spectator_internal " <> rest), _ ->
                html.text("🔍 Spectator " <> rest)
              Some(tag), None ->
                html.text("🔖 " <> api.format_pid(proc.pid) <> " " <> tag)
              Some(tag), Some(name) ->
                html.text(
                  "🔖 "
                  <> api.format_pid(proc.pid)
                  <> " "
                  <> tag
                  <> " registered as "
                  <> atom.to_string(name),
                )
              None, None ->
                html.text("🎛️ " <> api.format_pid(proc.pid) <> " Process")
              None, Some(name) ->
                html.text(
                  "📠 "
                  <> api.format_pid(proc.pid)
                  <> " "
                  <> atom.to_string(name),
                )
            },
            html.button(
              [
                attribute.class("panel-action suspend"),
                event.on_click(KillClicked(proc)),
              ],
              [html.text("🗡️ Kill")],
            ),
          ]),
          html.div([attribute.class("panel-content")], [
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
              html.dt([], [html.text("Status")]),
              html.dd([], [html.text(atom.to_string(proc.info.status))]),
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
                        attribute.class("panel-action resume"),
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
                        attribute.class("panel-action suspend"),
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
      html.div([attribute.class("footer-placeholder")], [
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
