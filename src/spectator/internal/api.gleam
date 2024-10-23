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
import gleam/uri

/// A tuple from Erlang that could be any size.
/// Used to represent ETS table data as it is not guaranteed to have a uniform number of columns.
pub type OpaqueTuple

/// The "SysState" type from the Erlang `sys` module, as returned by this function:
/// https://www.erlang.org/doc/apps/stdlib/sys.html#get_status/2
pub type SysState {
  ProcessRunning
  ProcessSuspended
}

/// A box for a pid, port or nif resource.
/// Used because some process info fields return lists of these types mixed together,
/// we distinguish them in the Erlang ffi and put them into these boxes for matching.
/// Used to describe items returned from
/// https://www.erlang.org/doc/apps/erts/erlang.html#process_info/2
/// as 'links', 'monitors' and 'monitored_by', also 'parent in 'get_details'
pub type SystemPrimitive {
  ProcessPrimitive(
    pid: process.Pid,
    name: Option(atom.Atom),
    tag: Option(String),
  )
  PortPrimitive(port_id: port.Port, name: Option(atom.Atom))
  RemoteProcessPrimitive(name: atom.Atom, node: atom.Atom)
  NifResourcePrimitive(dynamic.Dynamic)
}

/// A table in the Erlang ETS system.
/// Used as a box for information returned from
/// https://www.erlang.org/doc/apps/stdlib/ets.html#info/2
pub type Table {
  Table(
    id: erlang.Reference,
    name: atom.Atom,
    table_type: atom.Atom,
    size: Int,
    memory: Int,
    owner: SystemPrimitive,
    protection: atom.Atom,
    read_concurrency: Bool,
    write_concurrency: Bool,
  )
}

/// The data held by an ETS table.
pub type TableData {
  TableData(
    /// Each entry in the content list is a row in the table.
    /// Typically, all rows will have the same number of columns, but this is not guaranteed,
    /// therefore each row is again a list of dynamic data.
    content: List(List(dynamic.Dynamic)),
    /// The maximum number of columns in any row.
    max_length: Int,
  )
}

/// Box for data returned by the `sys:get_status` function.
/// https://www.erlang.org/doc/apps/stdlib/sys.html#get_status/2
/// Data is a bit tricky to extract, so we keep a remaining untyped
/// list of status_items, which could potentially be pretty printed.
pub type ProcessOtpStatus {
  ProcessOtpStatus(
    pid: process.Pid,
    module: atom.Atom,
    parent: process.Pid,
    sys_state: SysState,
    status_items: List(dynamic.Dynamic),
  )
}

/// A box for the details retuned for an OTP-compatible process 
pub type OtpDetails {
  OtpDetails(pid: process.Pid, status: ProcessOtpStatus, state: dynamic.Dynamic)
}

/// An inspected process with associated information,
/// boxed together with the process pid.
pub type ProcessItem {
  ProcessItem(pid: process.Pid, info: ProcessInfo)
}

/// Information about a process, as returned by `process_info/2`
pub type ProcessInfo {
  ProcessInfo(
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

/// Detailed information about a process
pub type ProcessDetails {
  ProcessDetails(
    messages: List(dynamic.Dynamic),
    links: List(SystemPrimitive),
    monitored_by: List(SystemPrimitive),
    monitors: List(SystemPrimitive),
    trap_exit: Bool,
    parent: Option(SystemPrimitive),
  )
}

/// An inspected port with associated information 
/// boxed together with the port id.
pub type PortItem {
  PortItem(port_id: port.Port, info: PortInfo)
}

/// Information about a port, as returned by `port_info/2`
pub type PortInfo {
  PortInfo(
    command_name: String,
    registered_name: Option(atom.Atom),
    connected_process: SystemPrimitive,
    os_pid: Option(Int),
    input: Int,
    output: Int,
    memory: Int,
    queue_size: Int,
  )
}

/// Detailed information about a port
pub type PortDetails {
  PortDetails(
    links: List(SystemPrimitive),
    monitored_by: List(SystemPrimitive),
    monitors: List(SystemPrimitive),
  )
}

/// The criteria by which to sort the list of processes
pub type ProcessSortCriteria {
  SortByProcessName
  SortByTag
  SortByCurrentFunction
  SortByProcessMemory
  SortByReductions
  SortByMessageQueue
  SortByProcessStatus
}

/// The criteria by which to sort the list of ETS tables
pub type TableSortCriteria {
  SortByTableId
  SortByTableName
  SortByTableType
  SortByTableSize
  SortByTableMemory
  SortByTableOwner
  SortByTableProtection
  SortByTableReadConcurrency
  SortByTableWriteConcurrency
}

pub type PortSortCriteria {
  SortByPortName
  SortByPortCommand
  SortByPortConnectedProcess
  SortByPortOsPid
  SortByPortInput
  SortByPortOutput
  SortByPortMemory
  SortByPortQueueSize
}

pub type SortDirection {
  Ascending
  Descending
}

/// System memory information as returned by 
/// https://www.erlang.org/doc/apps/erts/erlang.html#memory/0
/// The different values have the following relation to each other. 
/// Values beginning with an uppercase letter is not part of the result.
/// ```
/// total      = processes + system
/// processes  = processes_used + ProcessesNotUsed
/// system     = atom + binary + code + ets + OtherSystem
/// atom       = atom_used + AtomNotUsed
/// RealTotal  = processes + RealSystem
/// RealSystem = system + MissedSystem
/// ```
pub type MemoryStatistics {
  MemoryStatistics(
    total: Int,
    processes: Int,
    processes_used: Int,
    system: Int,
    atom: Int,
    atom_used: Int,
    binary: Int,
    code: Int,
    ets: Int,
  )
}

// ------ SORTING

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

pub fn sort_process_list(
  input: List(ProcessItem),
  criteria: ProcessSortCriteria,
  direction: SortDirection,
) -> List(ProcessItem) {
  case criteria {
    SortByProcessMemory -> {
      list.sort(input, fn(a, b) {
        int.compare(a.info.memory, b.info.memory) |> apply_direction(direction)
      })
    }
    SortByReductions -> {
      list.sort(input, fn(a, b) {
        int.compare(a.info.reductions, b.info.reductions)
        |> apply_direction(direction)
      })
    }
    SortByMessageQueue -> {
      list.sort(input, fn(a, b) {
        int.compare(a.info.message_queue_len, b.info.message_queue_len)
        |> apply_direction(direction)
      })
    }
    SortByCurrentFunction -> {
      list.sort(input, fn(a, b) {
        string.compare(
          function_to_string(a.info.current_function),
          function_to_string(b.info.current_function),
        )
        |> apply_direction(direction)
      })
    }
    SortByProcessName -> {
      list.sort(input, fn(a, b) {
        compare_name(a, b) |> apply_direction(direction)
      })
    }
    SortByTag -> {
      list.sort(input, fn(a, b) {
        compare_tag(a, b) |> apply_direction(direction)
      })
    }
    SortByProcessStatus -> {
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

fn disgustingly_index_into_list_at(
  in list: List(a),
  get index: Int,
) -> Result(a, Nil) {
  case index >= 0 {
    True ->
      list
      |> list.drop(index)
      |> list.first
    False -> Error(Nil)
  }
}

pub fn sort_table_data(
  input: TableData,
  sort_column: Int,
  sort_direction: SortDirection,
) -> TableData {
  // see it, say it,
  let sorted =
    list.sort(input.content, fn(a, b) {
      // Ok, but why?
      // Tables can have a variable number of columns, so we have to use lists
      // I still want to be able to sort by column index, so I have to index into the list
      // -> Let's see how performances fares here, we could otherwise go for a map or erlang array
      let a_cell = disgustingly_index_into_list_at(a, sort_column)
      let b_cell = disgustingly_index_into_list_at(b, sort_column)
      case a_cell, b_cell {
        Ok(a), Ok(b) ->
          compare_dynamic_data(a, b)
          |> apply_direction(sort_direction)
        Ok(_a), Error(_b) -> order.Gt |> apply_direction(sort_direction)
        Error(_a), Ok(_b) -> order.Lt
        Error(_), Error(_) -> order.Eq |> apply_direction(sort_direction)
      }
    })

  TableData(..input, content: sorted)
}

pub fn sort_table_list(
  input: List(Table),
  criteria: TableSortCriteria,
  direction: SortDirection,
) -> List(Table) {
  case criteria {
    SortByTableId -> {
      list.sort(input, fn(a, b) {
        string.compare(string.inspect(a.id), string.inspect(b.id))
        |> apply_direction(direction)
      })
    }
    SortByTableName -> {
      list.sort(input, fn(a, b) {
        string.compare(atom.to_string(a.name), atom.to_string(b.name))
        |> apply_direction(direction)
      })
    }
    SortByTableType -> {
      list.sort(input, fn(a, b) {
        string.compare(
          atom.to_string(a.table_type),
          atom.to_string(b.table_type),
        )
        |> apply_direction(direction)
      })
    }
    SortByTableSize -> {
      list.sort(input, fn(a, b) {
        int.compare(a.size, b.size) |> apply_direction(direction)
      })
    }
    SortByTableMemory -> {
      list.sort(input, fn(a, b) {
        int.compare(a.memory, b.memory) |> apply_direction(direction)
      })
    }
    SortByTableOwner -> {
      list.sort(input, fn(a, b) {
        string.compare(string.inspect(a.owner), string.inspect(b.owner))
        |> apply_direction(direction)
      })
    }
    SortByTableProtection -> {
      list.sort(input, fn(a, b) {
        string.compare(
          atom.to_string(a.protection),
          atom.to_string(b.protection),
        )
        |> apply_direction(direction)
      })
    }
    SortByTableReadConcurrency -> {
      list.sort(input, fn(a, b) {
        bool.compare(a.read_concurrency, b.read_concurrency)
        |> apply_direction(direction)
      })
    }
    SortByTableWriteConcurrency -> {
      list.sort(input, fn(a, b) {
        bool.compare(a.write_concurrency, b.write_concurrency)
        |> apply_direction(direction)
      })
    }
  }
}

pub fn sort_port_list(
  input: List(PortItem),
  criteria: PortSortCriteria,
  direction: SortDirection,
) -> List(PortItem) {
  case criteria {
    SortByPortName -> {
      list.sort(input, fn(a, b) {
        case a.info.registered_name, b.info.registered_name {
          Some(a), Some(b) ->
            string.compare(atom.to_string(a), atom.to_string(b))
          Some(_), None -> order.Gt
          None, Some(_) -> order.Lt
          None, None -> order.Eq
        }
      })
    }
    SortByPortCommand -> {
      list.sort(input, fn(a, b) {
        string.compare(a.info.command_name, b.info.command_name)
        |> apply_direction(direction)
      })
    }
    SortByPortConnectedProcess -> {
      list.sort(input, fn(a, b) {
        string.compare(
          string.inspect(a.info.connected_process),
          string.inspect(b.info.connected_process),
        )
        |> apply_direction(direction)
      })
    }
    SortByPortOsPid -> {
      list.sort(input, fn(a, b) {
        case a.info.os_pid, b.info.os_pid {
          Some(a), Some(b) -> int.compare(a, b)
          Some(_), None -> order.Gt
          None, Some(_) -> order.Lt
          None, None -> order.Eq
        }
        |> apply_direction(direction)
      })
    }
    SortByPortInput -> {
      list.sort(input, fn(a, b) {
        int.compare(a.info.input, b.info.input) |> apply_direction(direction)
      })
    }
    SortByPortOutput -> {
      list.sort(input, fn(a, b) {
        int.compare(a.info.output, b.info.output) |> apply_direction(direction)
      })
    }
    SortByPortMemory -> {
      list.sort(input, fn(a, b) {
        int.compare(a.info.memory, b.info.memory) |> apply_direction(direction)
      })
    }
    SortByPortQueueSize -> {
      list.sort(input, fn(a, b) {
        int.compare(a.info.queue_size, b.info.queue_size)
        |> apply_direction(direction)
      })
    }
  }
}

// ------ DATA FETCHING AND PROCESSING

// -------[PROCESS LIST]

pub fn get_process_list() -> List(ProcessItem) {
  list_processes()
  |> list.filter_map(fn(pid) {
    case get_process_info(pid) {
      Error(e) -> Error(e)
      Ok(info) -> Ok(ProcessItem(pid, info))
    }
  })
}

@external(erlang, "erlang", "processes")
pub fn list_processes() -> List(process.Pid)

@external(erlang, "spectator_ffi", "get_process_info")
pub fn get_process_info(
  pid: process.Pid,
) -> Result(ProcessInfo, dynamic.Dynamic)

// -------[PROCESS DETAILS]

@external(erlang, "spectator_ffi", "get_details")
pub fn get_details(pid: process.Pid) -> Result(ProcessDetails, dynamic.Dynamic)

// -------[OTP PROCESS DETAILS]

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

@external(erlang, "spectator_ffi", "get_status")
pub fn get_status(
  pid: process.Pid,
  timeout: Int,
) -> Result(ProcessOtpStatus, dynamic.Dynamic)

@external(erlang, "spectator_ffi", "get_state")
pub fn get_state(
  pid: process.Pid,
  timeout: Int,
) -> Result(dynamic.Dynamic, dynamic.Dynamic)

// -------[ETS]

pub fn get_ets_data(table: Table) {
  use raw_data <- result.try(get_raw_ets_data(table.id))
  process_raw_ets_data(raw_data, [], 0)
}

fn process_raw_ets_data(
  remainder: List(List(OpaqueTuple)),
  accumulator: List(List(dynamic.Dynamic)),
  max_length: Int,
) -> Result(TableData, Nil) {
  case remainder {
    [] -> {
      Ok(TableData(content: accumulator, max_length: max_length))
    }
    [[tup], ..rest] -> {
      let converted = opaque_tuple_to_list(tup)
      process_raw_ets_data(
        rest,
        [converted, ..accumulator],
        int.max(max_length, list.length(converted)),
      )
    }
    _ -> {
      Error(Nil)
    }
  }
}

@external(erlang, "spectator_ffi", "list_ets_tables")
pub fn list_ets_tables() -> Result(List(Table), Nil)

@external(erlang, "spectator_ffi", "get_ets_table_info")
pub fn get_ets_table_info(name: atom.Atom) -> Result(Table, Nil)

@external(erlang, "spectator_ffi", "get_ets_data")
pub fn get_raw_ets_data(
  table: erlang.Reference,
) -> Result(List(List(OpaqueTuple)), Nil)

@external(erlang, "spectator_ffi", "opaque_tuple_to_list")
pub fn opaque_tuple_to_list(tuple: OpaqueTuple) -> List(dynamic.Dynamic)

// -------[PORTS]

pub fn get_port_list() -> List(PortItem) {
  list_ports()
  |> list.filter_map(fn(pid) {
    case get_port_info(pid) {
      Error(e) -> Error(e)
      Ok(info) -> Ok(PortItem(pid, info))
    }
  })
}

@external(erlang, "erlang", "ports")
pub fn list_ports() -> List(port.Port)

@external(erlang, "spectator_ffi", "get_port_info")
pub fn get_port_info(port: port.Port) -> Result(PortInfo, dynamic.Dynamic)

@external(erlang, "spectator_ffi", "get_port_details")
pub fn get_port_details(port: port.Port) -> Result(PortDetails, dynamic.Dynamic)

// ------- [SYSTEM STATISTICS]

@external(erlang, "spectator_ffi", "get_memory_statistics")
pub fn get_memory_statistics() -> Result(MemoryStatistics, dynamic.Dynamic)

// ------ FORMATTING

fn function_to_string(f: #(atom.Atom, atom.Atom, Int)) {
  atom.to_string(f.0) <> atom.to_string(f.1) <> int.to_string(f.2)
}

@external(erlang, "spectator_ffi", "format_pid")
pub fn format_pid(pid: process.Pid) -> String

@external(erlang, "spectator_ffi", "format_port")
pub fn format_port(port: port.Port) -> String

// ------- SYSTEM INTERACTION

@external(erlang, "sys", "suspend")
pub fn suspend(pid: process.Pid) -> Nil

@external(erlang, "sys", "resume")
pub fn resume(pid: process.Pid) -> Nil

@external(erlang, "ets", "insert")
pub fn ets_insert(table: atom.Atom, tuple: List(#(k, v))) -> Nil

@external(erlang, "spectator_ffi", "new_ets_table")
pub fn new_ets_table(name: atom.Atom) -> Result(atom.Atom, Nil)

@external(erlang, "spectator_ffi", "pid_to_string")
fn pid_to_string(pid: process.Pid) -> String

pub fn serialize_pid(pid: process.Pid) -> String {
  pid_to_string(pid)
  |> uri.percent_encode
}

@external(erlang, "spectator_ffi", "port_to_string")
fn port_to_string(port: port.Port) -> String

pub fn serialize_port(port: port.Port) -> String {
  port_to_string(port)
  |> uri.percent_encode
}

@external(erlang, "spectator_ffi", "pid_from_string")
fn pid_from_string(string: String) -> Result(process.Pid, Nil)

pub fn decode_pid(string: String) -> Result(process.Pid, Nil) {
  use decoded <- result.try(uri.percent_decode(string))
  pid_from_string(decoded)
}

@external(erlang, "spectator_ffi", "port_from_string")
fn port_from_string(string: String) -> Result(port.Port, Nil)

pub fn decode_port(string: String) -> Result(port.Port, Nil) {
  use decoded <- result.try(uri.percent_decode(string))
  port_from_string(decoded)
}

// ------- SYSTEM INFORMATION

@external(erlang, "spectator_ffi", "compare_data")
pub fn compare_dynamic_data(
  a: dynamic.Dynamic,
  b: dynamic.Dynamic,
) -> order.Order

@external(erlang, "spectator_ffi", "get_word_size")
pub fn get_word_size() -> Int

// ------- TAG MANAGER GEN_SERVER

@external(erlang, "spectator_tag_manager", "start_link")
pub fn start_tag_manager() -> Result(process.Pid, dynamic.Dynamic)

@external(erlang, "spectator_tag_manager", "add_tag")
pub fn add_tag(pid: process.Pid, tag: String) -> Nil

@external(erlang, "spectator_tag_manager", "get_tag")
pub fn get_tag(pid: process.Pid) -> Option(String)
