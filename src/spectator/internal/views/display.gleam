/// View functions for displaying all kinds of data from the system
import gleam/string
import gleam/erlang
import gleam/erlang/atom
import gleam/erlang/port
import gleam/erlang/process
import gleam/int
import lustre/element/html
import spectator/internal/api

pub fn pid(pid: process.Pid) {
  html.text("PID" <> api.format_pid(pid))
}

pub fn port(port: port.Port) {
  html.text(api.format_port(port))
}

pub fn atom(a: atom.Atom) {
  html.text(atom.to_string(a))
}

pub fn reference(ref: erlang.Reference) {
  // TODO: Implement inspect for Reference via FFI
  html.text(string.inspect(ref))
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

pub fn system_primitive(primitive: api.SystemPrimitive) {
  case primitive {
    api.Process(p) -> pid(p)
    api.RegisteredProcess(name) -> atom(name)
    api.Port(p) -> port(p)
    api.RegisteredPort(name) -> atom(name)
    api.NifResource(_) -> html.text("NIF Resource")
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
