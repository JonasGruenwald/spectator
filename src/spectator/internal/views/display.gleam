/// View functions for displaying all kinds of data from the system
import gleam/dynamic
import gleam/erlang
import gleam/erlang/atom
import gleam/erlang/port
import gleam/erlang/process
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string
import lustre/attribute
import lustre/element/html
import lustre/event
import spectator/internal/api

pub fn pid(pid: process.Pid) {
  html.text(api.format_pid(pid))
}

pub fn pid_button(
  pid: process.Pid,
  name: Option(atom.Atom),
  tag: Option(String),
  on_click: fn(process.Pid) -> a,
) {
  case name, tag {
    None, None ->
      html.button(
        [
          event.on_click(on_click(pid)),
          attribute.class("interactive-primitive"),
        ],
        [html.text(api.format_pid(pid))],
      )
    Some(name), None ->
      html.button(
        [
          event.on_click(on_click(pid)),
          attribute.class("interactive-primitive named"),
          attribute.title("PID" <> api.format_pid(pid)),
        ],
        [html.text(atom.to_string(name))],
      )
    None, Some("__spectator_internal" <> internal_tag) ->
      html.button(
        [
          event.on_click(on_click(pid)),
          attribute.class("interactive-primitive muted"),
          attribute.title("This process is used for the Spectator application"),
        ],
        [html.text("🔍" <> internal_tag <> api.format_pid(pid))],
      )
    None, Some(tag) ->
      html.button(
        [
          event.on_click(on_click(pid)),
          attribute.class("interactive-primitive tagged"),
        ],
        [html.text("🔖 " <> tag <> api.format_pid(pid))],
      )
    Some(name), Some(tag) ->
      html.button(
        [
          event.on_click(on_click(pid)),
          attribute.class("interactive-primitive named tagged"),
          attribute.title("PID" <> api.format_pid(pid)),
        ],
        [html.text("🔖 " <> tag <> "<" <> atom.to_string(name) <> ">")],
      )
  }
}

pub fn pid_link(pid: process.Pid, name: Option(atom.Atom), tag: Option(String)) {
  case name, tag {
    None, None ->
      html.a(
        [
          attribute.href("/processes?selected=" <> api.serialize_pid(pid)),
          attribute.class("interactive-primitive"),
        ],
        [html.text(api.format_pid(pid))],
      )
    Some(name), None ->
      html.a(
        [
          attribute.href("/processes?selected=" <> api.serialize_pid(pid)),
          attribute.class("interactive-primitive named"),
          attribute.title("PID" <> api.format_pid(pid)),
        ],
        [html.text(atom.to_string(name))],
      )
    None, Some("__spectator_internal" <> internal_tag) ->
      html.a(
        [
          attribute.href("/processes?selected=" <> api.serialize_pid(pid)),
          attribute.class("interactive-primitive muted"),
          attribute.title("This process is used for the Spectator application"),
        ],
        [html.text("🔍" <> internal_tag <> api.format_pid(pid))],
      )
    None, Some(tag) ->
      html.a(
        [
          attribute.href("/processes?selected=" <> api.serialize_pid(pid)),
          attribute.class("interactive-primitive tagged"),
        ],
        [html.text("🔖 " <> tag <> api.format_pid(pid))],
      )
    Some(name), Some(tag) ->
      html.a(
        [
          attribute.href("/processes?selected=" <> api.serialize_pid(pid)),
          attribute.class("interactive-primitive named tagged"),
          attribute.title("PID" <> api.format_pid(pid)),
        ],
        [html.text("🔖 " <> tag <> "<" <> atom.to_string(name) <> ">")],
      )
  }
}

pub fn port(port: port.Port) {
  html.text(api.format_port(port))
}

pub fn port_link(port: port.Port, name: Option(atom.Atom)) {
  let label = case name {
    None -> api.format_port(port)
    Some(n) -> api.format_port(port) <> " (" <> atom.to_string(n) <> ")"
  }
  html.a(
    [
      attribute.class("interactive-primitive"),
      attribute.href("/ports?selected=" <> api.serialize_port(port)),
    ],
    [html.text(label)],
  )
}

pub fn inspect(d: dynamic.Dynamic) {
  html.text(string.inspect(d))
}

pub fn atom(a: atom.Atom) {
  html.text(atom.to_string(a))
}

pub fn reference(ref: erlang.Reference) {
  html.text(string.inspect(ref))
}

pub fn storage_words(words: Int, word_size: Int) {
  storage(words * word_size)
}

pub fn storage(size: Int) {
  case size {
    _s if size < 1024 -> html.text(int.to_string(size) <> " B")
    _s if size < 1_048_576 -> html.text(int.to_string(size / 1024) <> " KiB")
    _s -> html.text(int.to_string(size / 1024 / 1024) <> " MiB")
  }
}

pub fn storage_detailed(size) {
  let byte_size = int.to_string(size) <> " Bytes"
  case size {
    _s if size < 1024 -> html.text(byte_size)
    _s if size < 1_048_576 ->
      html.text(byte_size <> " (" <> int.to_string(size / 1024) <> " KiB)")
    _s ->
      html.text(
        byte_size <> " (" <> int.to_string(size / 1024 / 1024) <> " MiB)",
      )
  }
}

pub fn bool(b: Bool) {
  case b {
    True -> html.text("true")
    False -> html.text("false")
  }
}

pub fn named_remote_process(name: atom.Atom, node: atom.Atom) {
  html.text(atom.to_string(name) <> " on " <> atom.to_string(node))
}

pub fn system_primitive_interactive(
  primitive: api.SystemPrimitive,
  on_process_click: fn(process.Pid) -> a,
) {
  case primitive {
    api.ProcessPrimitive(pid:, name:, tag:) ->
      pid_button(pid, name, tag, on_process_click)
    api.RemoteProcessPrimitive(name:, node:) -> named_remote_process(name, node)
    api.PortPrimitive(port_id:, name:) -> port_link(port_id, name)
    api.NifResourcePrimitive(_) -> html.text("NIF Res.")
  }
}

pub fn system_primitive(primitive: api.SystemPrimitive) {
  case primitive {
    api.ProcessPrimitive(pid:, name:, tag:) -> pid_link(pid, name, tag)
    api.RemoteProcessPrimitive(name:, node:) -> named_remote_process(name, node)
    api.PortPrimitive(port_id:, name:) -> port_link(port_id, name)
    api.NifResourcePrimitive(_) -> html.text("NIF Res.")
  }
}

pub fn function(ref: #(atom.Atom, atom.Atom, Int)) {
  let #(module, function, arity) = ref

  html.text(
    atom.to_string(module)
    <> ":"
    <> atom.to_string(function)
    <> "/"
    <> int.to_string(arity),
  )
}

pub fn number(n: Int) {
  html.text(int.to_string(n))
}
