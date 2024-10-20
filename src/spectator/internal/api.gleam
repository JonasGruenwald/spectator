import gleam/bool
import gleam/dynamic
import gleam/erlang
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

pub type Table {
  Table(
    id: erlang.Reference,
    name: atom.Atom,
    table_type: atom.Atom,
    size: Int,
    memory: Int,
    owner: process.Pid,
    protection: atom.Atom,
    read_concurrency: Bool,
    write_concurrency: Bool,
  )
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
  ProcessStatus
}

pub type TableSortCriteria {
  TableId
  TableName
  TableType
  Size
  TableMemory
  Owner
  Protection
  ReadConcurrency
  WriteConcurrency
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
    status: atom.Atom,
  )
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
    ProcessStatus -> {
      list.sort(input, fn(a, b) {
        string.compare(
          atom.to_string(a.info.status),
          atom.to_string(b.info.status),
        )
        |> apply_direction(direction)
      })
    }
  }
}

pub fn sort_table_list(
  input: List(Table),
  criteria: TableSortCriteria,
  direction: SortDirection,
) -> List(Table) {
  case criteria {
    TableId -> {
      list.sort(input, fn(a, b) {
        string.compare(string.inspect(a.id), string.inspect(b.id))
        |> apply_direction(direction)
      })
    }
    TableName -> {
      list.sort(input, fn(a, b) {
        string.compare(atom.to_string(a.name), atom.to_string(b.name))
        |> apply_direction(direction)
      })
    }
    TableType -> {
      list.sort(input, fn(a, b) {
        string.compare(
          atom.to_string(a.table_type),
          atom.to_string(b.table_type),
        )
        |> apply_direction(direction)
      })
    }
    Size -> {
      list.sort(input, fn(a, b) {
        int.compare(a.size, b.size) |> apply_direction(direction)
      })
    }
    TableMemory -> {
      list.sort(input, fn(a, b) {
        int.compare(a.memory, b.memory) |> apply_direction(direction)
      })
    }
    Owner -> {
      list.sort(input, fn(a, b) {
        string.compare(string.inspect(a.owner), string.inspect(b.owner))
        |> apply_direction(direction)
      })
    }
    Protection -> {
      list.sort(input, fn(a, b) {
        string.compare(
          atom.to_string(a.protection),
          atom.to_string(b.protection),
        )
        |> apply_direction(direction)
      })
    }
    ReadConcurrency -> {
      list.sort(input, fn(a, b) {
        bool.compare(a.read_concurrency, b.read_concurrency)
        |> apply_direction(direction)
      })
    }
    WriteConcurrency -> {
      list.sort(input, fn(a, b) {
        bool.compare(a.write_concurrency, b.write_concurrency)
        |> apply_direction(direction)
      })
    }
  }
}

// Direct bindings to Erlang APIs

@external(erlang, "sys", "suspend")
pub fn suspend(pid: process.Pid) -> Nil

@external(erlang, "sys", "resume")
pub fn resume(pid: process.Pid) -> Nil

@external(erlang, "ets", "insert")
pub fn ets_insert(table: atom.Atom, tuple: List(#(k, v))) -> Nil

// Spectator FFI

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

@external(erlang, "spectator_ffi", "format_pid")
pub fn format_pid(pid: process.Pid) -> String

@external(erlang, "spectator_ffi", "format_port")
pub fn format_port(port: port.Port) -> String

@external(erlang, "spectator_ffi", "list_ets_tables")
pub fn list_ets_tables() -> Result(List(Table), Nil)

@external(erlang, "spectator_ffi", "new_ets_table")
pub fn new_ets_table(name: atom.Atom) -> Result(atom.Atom, Nil)

@external(erlang, "spectator_ffi", "get_ets_data")
pub fn get_ets_data(
  table: atom.Atom,
) -> Result(List(List(#(dynamic.Dynamic, dynamic.Dynamic))), Nil)

// Tag manager (gen_server)

@external(erlang, "spectator_tag_manager", "start_link")
pub fn start_tag_manager() -> Result(process.Pid, dynamic.Dynamic)

@external(erlang, "spectator_tag_manager", "add_tag")
pub fn add_tag(pid: process.Pid, tag: String) -> Nil

@external(erlang, "spectator_tag_manager", "get_tag")
pub fn get_tag(pid: process.Pid) -> Option(String)
