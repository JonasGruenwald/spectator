/// View functions for displaying all kinds of data from the system
import gleam/dynamic
import gleam/erlang
import gleam/erlang/atom
import gleam/erlang/port
import gleam/erlang/process
import gleam/int
import gleam/string
import lustre/attribute
import lustre/element/html
import lustre/event
import spectator/internal/api

pub fn pid(pid: process.Pid) {
  html.text("PID" <> api.format_pid(pid))
}

pub fn pid_button(pid: process.Pid, on_click: fn(process.Pid) -> a) {
  html.button(
    [event.on_click(on_click(pid)), attribute.class("pid-interactive")],
    [html.text("PID" <> api.format_pid(pid))],
  )
}

pub fn port(port: port.Port) {
  html.text(api.format_port(port))
}

pub fn inspect(d: dynamic.Dynamic) {
  html.text(string.inspect(d))
}

pub fn atom(a: atom.Atom) {
  html.text(atom.to_string(a))
}

pub fn reference(ref: erlang.Reference) {
  // TODO: Implement inspect for Reference via FFI
  html.text(string.inspect(ref))
}

pub fn storage_words(words: Int) {
  storage(words * api.get_word_size())
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

pub fn system_primitive_interactive(
  primitive: api.SystemPrimitive,
  on_click: fn(api.SystemPrimitive) -> a,
) {
  case primitive {
    api.Process(p) -> pid_button(p, fn(pid) { on_click(api.Process(pid)) })
    api.RegisteredProcess(name) -> atom(name)
    api.Port(p) -> port(p)
    api.RegisteredPort(name) -> atom(name)
    api.NifResource(_) -> html.text("NIF Res")
  }
}

pub fn system_primitive(primitive: api.SystemPrimitive) {
  case primitive {
    api.Process(p) -> pid(p)
    api.RegisteredProcess(name) -> atom(name)
    api.Port(p) -> port(p)
    api.RegisteredPort(name) -> atom(name)
    api.NifResource(_) -> html.text("NIF Res")
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
