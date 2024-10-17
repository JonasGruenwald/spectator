import gleam/dynamic
import gleam/erlang/atom
import gleam/erlang/process
import gleam/io
import gleam/list
import gleam/option
import gleam/result

pub type SysState {
  Running
  Suspended
}

pub type SpectatorDebugTag {
  SpectatorDebugTag
}

pub type Status {
  Status(
    pid: process.Pid,
    module: atom.Atom,
    parent: process.Pid,
    sys_state: SysState,
    status_items: List(dynamic.Dynamic),
  )
}

pub type Module {
  Module(atom.Atom)
}

pub type Function {
  Function(atom.Atom)
}

pub type Arity {
  Arity(Int)
}

pub type ProcessItem {
  ProcessItem(pid: process.Pid, info: Info)
}

pub type Info {
  Info(
    /// tuple of module, function, and arity.
    current_function: #(atom.Atom, atom.Atom, Int),
    /// tuple of module, function, and arity.
    initial_call: #(atom.Atom, atom.Atom, Int),
    registered_name: option.Option(atom.Atom),
    memory: Int,
    message_queue_len: Int,
    reductions: Int,
    tag: option.Option(String),
  )
}

pub fn add_tag(tag: String) {
  put_into_process_dictionary(SpectatorDebugTag, tag)
}

pub fn get_info_list() -> Result(List(ProcessItem), dynamic.Dynamic) {
  list_processes()
  |> list.map(fn(pid) {
    case get_info(pid) {
      Error(e) -> Error(e) |> io.debug()
      Ok(info) -> Ok(ProcessItem(pid, info)) |> io.debug()
    }
  })
  |> result.all
}

@external(erlang, "erlang", "put")
fn put_into_process_dictionary(a: a, b: b) -> Nil

@external(erlang, "erlang", "processes")
pub fn list_processes() -> List(process.Pid)

@external(erlang, "spectator_ffi", "get_status")
pub fn get_status(
  pid: process.Pid,
  timeout: Int,
) -> Result(Status, dynamic.Dynamic)

@external(erlang, "spectator_ffi", "get_info")
pub fn get_info(pid: process.Pid) -> Result(Info, dynamic.Dynamic)

@external(erlang, "spectator_ffi", "get_all_info")
pub fn get_all_info(
  pid: process.Pid,
) -> Result(List(dynamic.Dynamic), dynamic.Dynamic)

@external(erlang, "spectator_ffi", "get_state")
pub fn get_state(
  pid: process.Pid,
  timeout: Int,
) -> Result(dynamic.Dynamic, dynamic.Dynamic)

@external(erlang, "sys", "suspend")
pub fn suspend(pid: process.Pid) -> Nil

@external(erlang, "sys", "resume")
pub fn resume(pid: process.Pid) -> Nil

@external(erlang, "spectator_ffi", "format_pid")
pub fn format_pid(pid: process.Pid) -> String
