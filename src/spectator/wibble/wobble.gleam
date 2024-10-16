import gleam/io
import stacky

pub fn hello() {
  stacky.trace()
  |> stacky.frame(1)
  |> stacky.erlang_module_name()
  |> io.debug()
}

