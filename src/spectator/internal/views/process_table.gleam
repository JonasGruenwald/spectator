import gleam/erlang/atom
import gleam/list
import gleam/option
import lustre/attribute
import lustre/element/html
import spectator/internal/api
import spectator/internal/views/display

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

pub fn render(processes: List(api.ProcessItem)) {
  html.table([], [
    html.thead([], [
      html.tr([], [
        html.th([
          attribute.title("Process PID or registered name")], [
          html.text("Name"),
        ]),
        html.th([attribute.title("Spectator tag or initial call")], [
          html.text("Tag"),
        ]),
        html.th([], [html.text("Current")]),
        html.th([], [html.text("Reductions")]),
        html.th([], [html.text("Memory")]),
        html.th([], [html.text("Queue")]),
      ]),
    ]),
    html.tbody(
      [],
      list.map(processes, fn(process) {
        html.tr([], [
          html.td([], [render_name(process)]),
          html.td([], [render_tag(process)]),
          html.td([], [display.function(process.info.current_function)]),
          html.td([], [display.number(process.info.reductions)]),
          html.td([], [display.number(process.info.memory)]),
          html.td([], [display.number(process.info.message_queue_len)]),
        ])
      }),
    ),
  ])
}
