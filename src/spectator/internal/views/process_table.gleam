import gleam/dynamic
import gleam/erlang/atom
import gleam/list
import gleam/option.{type Option, None, Some}
import lustre/attribute
import lustre/element/html
import lustre/event
import pprint
import spectator/internal/api
import spectator/internal/views/display
import spectator/internal/views/icon

fn render_name(process: api.ProcessItem) {
  case process.info.registered_name {
    option.None -> display.pid(process.pid)
    option.Some(name) -> html.text(atom.to_string(name))
  }
}

fn render_tag(process: api.ProcessItem) {
  case process.info.tag {
    option.None -> display.function(process.info.initial_call)
    option.Some(tag) -> html.text(tag)
  }
}

fn render_primitive_list(primitives: List(api.SystemPrimitive)) {
  list.map(primitives, display.system_primitive)
  |> list.intersperse(html.text(", "))
}

fn render_details(
  p: Option(api.ProcessItem),
  d: Option(api.ProcessDetails),
  status: Option(api.Status),
  state: Option(dynamic.Dynamic),
) {
  case p, d {
    Some(proc), Some(details) ->
      html.div([attribute.class("details")], [
        html.div([attribute.class("general")], [
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
            html.dd([], [html.text(atom.to_string(details.status))]),
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
          html.dl([], [
            html.dt([], [html.text("Parent")]),
            html.dd([], [
              case details.parent {
                option.None -> html.text("None")
                option.Some(pid) -> display.pid(pid)
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
            html.dd([], [display.number(proc.info.memory)]),
          ]),
          html.dl([], [
            html.dt([], [html.text("Message Queue Length")]),
            html.dd([], [display.number(proc.info.message_queue_len)]),
          ]),
        ]),
        html.div([attribute.class("otp")], case status, state {
          Some(status), Some(state) -> {
            [
              html.div([], [
                html.strong([], [html.text("OTP Process: ")]),
                display.atom(status.module),
              ]),
              html.div([attribute.class("state-container")], [
                html.pre([], [html.text(pprint.format(state))]),
              ]),
            ]
          }
          _, _ -> [
            html.text("This process does not appear to be OTP-compliant"),
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
  case active {
    Some(active) if active.pid == process.pid -> attribute.class("selected")
    _ -> attribute.none()
  }
}

fn heading(
  name: String,
  title: String,
  sort_criteria: api.InfoSortCriteria,
  current_sort_criteria: api.InfoSortCriteria,
  current_sort_direction: api.SortDirection,
  handle_heading_click: fn(api.InfoSortCriteria) -> a,
) {
  html.th(
    [
      attribute.title(title),
      event.on_click(handle_heading_click(sort_criteria)),
    ],
    [
      html.div([], [
        case current_sort_criteria == sort_criteria {
          True -> {
            case current_sort_direction {
              api.Ascending -> icon.sort_ascending()
              api.Descending -> icon.sort_descending()
            }
          }
          False -> html.text("")
        },
        html.text(name),
      ]),
    ],
  )
}

pub fn render(
  processes: List(api.ProcessItem),
  handle_process_click: fn(api.ProcessItem) -> a,
  active: Option(api.ProcessItem),
  details: Option(api.ProcessDetails),
  status: Option(api.Status),
  state: Option(dynamic.Dynamic),
  sort_criteria: api.InfoSortCriteria,
  sort_direction: api.SortDirection,
  handle_heading_click: fn(api.InfoSortCriteria) -> a,
) {
  html.table([], [
    html.thead([], [
      html.tr([], [
        heading(
          "Name",
          "Process PID or registered name",
          api.Name,
          sort_criteria,
          sort_direction,
          handle_heading_click,
        ),
        heading(
          "Tag",
          "Spectator tag or initial call",
          api.Tag,
          sort_criteria,
          sort_direction,
          handle_heading_click,
        ),
        heading(
          "Current",
          "Current function",
          api.CurrentFunction,
          sort_criteria,
          sort_direction,
          handle_heading_click,
        ),
        heading(
          "Reductions",
          "Number of reductions",
          api.Reductions,
          sort_criteria,
          sort_direction,
          handle_heading_click,
        ),
        heading(
          "Memory",
          "Memory usage",
          api.Memory,
          sort_criteria,
          sort_direction,
          handle_heading_click,
        ),
        heading(
          "Memory",
          "Message queue size",
          api.MessageQueue,
          sort_criteria,
          sort_direction,
          handle_heading_click,
        ),
      ]),
    ]),
    html.tbody(
      [],
      list.map(processes, fn(process) {
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
            html.td([], [display.number(process.info.memory)]),
            html.td([], [display.number(process.info.message_queue_len)]),
          ],
        )
      }),
    ),
    html.tfoot([], [
      html.tr([], [
        html.td([attribute.attribute("colspan", "6")], [
          render_details(active, details, status, state),
        ]),
      ]),
    ]),
  ])
}
