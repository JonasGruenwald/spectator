import gleam/erlang/atom
import gleam/erlang/process
import gleam/int
import gleam/string
import lustre/element/html

pub fn pid(pid: process.Pid) {
  html.text(string.inspect(pid))
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
