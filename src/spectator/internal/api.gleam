import gleam/dynamic
import gleam/erlang/atom
import gleam/erlang/port
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/string

pub type SysState {
  Running
  Suspended
}

pub type SystemPrimitive {
  Process(process.Pid)
  RegisteredProcess(atom.Atom)
  Port(port.Port)
  RegisteredPort(atom.Atom)
  NifResource(dynamic.Dynamic)
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

pub type OtpDetails {
  OtpDetails(pid: process.Pid, status: Status, state: dynamic.Dynamic)
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

pub type ProcessDetails {
  ProcessDetails(
    messages: List(dynamic.Dynamic),
    links: List(SystemPrimitive),
    monitored_by: List(SystemPrimitive),
    monitors: List(SystemPrimitive),
    status: atom.Atom,
    trap_exit: Bool,
    parent: Option(process.Pid),
  )
}

pub type InfoSortCriteria {
  Name
  Tag
  CurrentFunction
  Memory
  Reductions
  MessageQueue
}

pub type SortDirection {
  Ascending
  Descending
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

pub fn get_info_list() -> List(ProcessItem) {
  list_processes()
  |> list.filter_map(fn(pid) {
    case get_info(pid) {
      Error(e) -> Error(e)
      Ok(info) -> Ok(ProcessItem(pid, info))
    }
  })
}

fn function_to_string(f: #(atom.Atom, atom.Atom, Int)) {
  atom.to_string(f.0) <> atom.to_string(f.1) <> int.to_string(f.2)
}

fn compare_name(a: ProcessItem, b: ProcessItem) -> order.Order {
  case a.info.registered_name, b.info.registered_name {
    Some(a_name), Some(b_name) ->
      string.compare(atom.to_string(a_name), atom.to_string(b_name))
    // Names are greater than PIDs
    None, Some(_) -> order.Lt
    Some(_), None -> order.Gt
    // All PIDs are considered equal for sorting purposes
    None, None -> order.Eq
  }
}

fn compare_tag(a: ProcessItem, b: ProcessItem) -> order.Order {
  case a.info.tag, b.info.tag {
    Some(a_tag), Some(b_tag) -> string.compare(a_tag, b_tag)
    // Tags are greater than initial calls
    None, Some(_) -> order.Lt
    Some(_), None -> order.Gt
    // If no tag, we compare initial calls
    None, None ->
      string.compare(
        function_to_string(a.info.initial_call),
        function_to_string(b.info.initial_call),
      )
  }
}

pub fn invert_sort_direction(direction: SortDirection) -> SortDirection {
  case direction {
    Ascending -> Descending
    Descending -> Ascending
  }
}

fn apply_direction(order: order.Order, direction: SortDirection) -> order.Order {
  case direction {
    Ascending -> order
    Descending -> order.negate(order)
  }
}

pub fn request_otp_data(p: process.Pid, callback: fn(OtpDetails) -> Nil) {
  process.start(
    fn() {
      case get_status(p, 100) {
        Error(_) -> {
          Nil
        }
        Ok(status) -> {
          let state =
            get_state(p, 100)
            |> result.unwrap(dynamic.from(option.None))
          callback(OtpDetails(pid: p, status:, state:))
        }
      }
    },
    False,
  )
}

pub fn sort_info_list(
  input: List(ProcessItem),
  criteria: InfoSortCriteria,
  direction: SortDirection,
) -> List(ProcessItem) {
  case criteria {
    Memory -> {
      list.sort(input, fn(a, b) {
        int.compare(a.info.memory, b.info.memory) |> apply_direction(direction)
      })
    }
    Reductions -> {
      list.sort(input, fn(a, b) {
        int.compare(a.info.reductions, b.info.reductions)
        |> apply_direction(direction)
      })
    }
    MessageQueue -> {
      list.sort(input, fn(a, b) {
        int.compare(a.info.message_queue_len, b.info.message_queue_len)
        |> apply_direction(direction)
      })
    }
    CurrentFunction -> {
      list.sort(input, fn(a, b) {
        string.compare(
          function_to_string(a.info.current_function),
          function_to_string(b.info.current_function),
        )
        |> apply_direction(direction)
      })
    }
    Name -> {
      list.sort(input, fn(a, b) {
        compare_name(a, b) |> apply_direction(direction)
      })
    }
    Tag -> {
      list.sort(input, fn(a, b) {
        compare_tag(a, b) |> apply_direction(direction)
      })
    }
  }
}

@external(erlang, "erlang", "put")
pub fn put_into_process_dictionary(a: a, b: b) -> Nil

@external(erlang, "spectator_ffi", "get_status")
pub fn get_status(
  pid: process.Pid,
  timeout: Int,
) -> Result(Status, dynamic.Dynamic)

@external(erlang, "erlang", "processes")
pub fn list_processes() -> List(process.Pid)

@external(erlang, "spectator_ffi", "get_info")
pub fn get_info(pid: process.Pid) -> Result(Info, dynamic.Dynamic)

@external(erlang, "spectator_ffi", "get_details")
pub fn get_details(pid: process.Pid) -> Result(ProcessDetails, dynamic.Dynamic)

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

@external(erlang, "spectator_ffi", "format_port")
pub fn format_port(port: port.Port) -> String
