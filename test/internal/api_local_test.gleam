//// Test cases that run api requests against the local node

import carpenter/table
import gleam/dynamic
import gleam/erlang/atom
import gleam/erlang/port
import gleam/erlang/process
import gleam/function
import gleam/list
import gleam/option.{None, Some}
import gleam/set
import gleeunit/should
import spectator/internal/api
import utils/pantry

// ------ SORTING

pub fn invert_sort_direction_test() {
  api.invert_sort_direction(api.Ascending)
  |> should.equal(api.Descending)

  api.invert_sort_direction(api.Descending)
  |> should.equal(api.Ascending)
}

//  [...]

// ------ DATA FETCHING AND PROCESSING

// -------[PROCESS LIST]

pub fn get_process_list_test() {
  let assert Ok(pantry_actor) = pantry.new()
  let assert Ok(pantry_pid) = process.subject_owner(pantry_actor)

  let sample =
    api.get_process_list(None)
    |> list.find(fn(pi) { pi.pid == pantry_pid })
    |> should.be_ok

  sample.info.current_function
  |> should.equal(#(atom.create("gleam_erlang_ffi"), atom.create("select"), 2))

  sample.info.initial_call
  |> should.equal(#(atom.create("erlang"), atom.create("apply"), 2))

  sample.info.registered_name
  |> should.be_none()

  sample.info.tag
  |> should.be_none()

  sample.info.status
  |> should.equal(atom.create("waiting"))

  dynamic.int(sample.info.memory)
  |> dynamic.classify()
  |> should.equal("Int")

  dynamic.int(sample.info.message_queue_len)
  |> dynamic.classify()
  |> should.equal("Int")

  dynamic.int(sample.info.reductions)
  |> dynamic.classify()
  |> should.equal("Int")
}

pub fn list_processes_test() {
  // Start a process
  let assert Ok(sub) = pantry.new()
  let assert Ok(pid) = process.subject_owner(sub)

  // Check that the process is in the list of processes
  api.list_processes(None)
  |> should.be_ok
  |> list.find(fn(p) { p == pid })
  |> should.be_ok
}

pub fn get_process_info_test() {
  // Start a process
  let assert Ok(sub) = pantry.new()
  let assert Ok(pid) = process.subject_owner(sub)

  // Retrieve the process info for that process
  let info =
    api.get_process_info(None, pid)
    |> should.be_ok

  info.current_function
  |> should.equal(#(atom.create("gleam_erlang_ffi"), atom.create("select"), 2))

  info.initial_call
  |> should.equal(#(atom.create("erlang"), atom.create("apply"), 2))

  info.registered_name
  |> should.be_none()

  info.tag
  |> should.be_none()

  info.status
  |> should.equal(atom.create("waiting"))

  dynamic.int(info.memory)
  |> dynamic.classify()
  |> should.equal("Int")

  dynamic.int(info.message_queue_len)
  |> dynamic.classify()
  |> should.equal("Int")

  dynamic.int(info.reductions)
  |> dynamic.classify()
  |> should.equal("Int")
}

pub fn get_process_info_failure_test() {
  // If this test is flakey, think of something else
  // (or remove it hehe)
  let assert Ok(pid) = api.decode_pid("<0.255.0>")
  api.get_process_info(None, pid)
  |> should.be_error
}

// -------[PROCESS DETAILS]

pub fn get_details_test() {
  // Start a process
  let assert Ok(sub) = pantry.new()
  let assert Ok(pid) = process.subject_owner(sub)
  let details =
    api.get_details(None, pid)
    |> should.be_ok()

  case details.parent {
    Some(api.ProcessPrimitive(pid, _, _)) -> {
      pid
      |> should.equal(process.self())
    }
    _ -> {
      should.fail()
    }
  }

  case details.links {
    [api.ProcessPrimitive(pid, _, _)] -> {
      pid
      |> should.equal(process.self())
    }
    _ -> {
      should.fail()
    }
  }
}

// -------[OTP PROCESS DETAILS]

pub fn get_status_test() {
  // Start an OTP actor
  let assert Ok(sub) = pantry.new()
  let assert Ok(pid) = process.subject_owner(sub)

  // Retrieve the status for that actor
  let status =
    api.get_status(None, pid, 500)
    |> should.be_ok

  let actor_module_name = atom.create("gleam@otp@actor")

  status.module
  |> should.equal(actor_module_name)

  status.pid
  |> should.equal(pid)

  status.parent
  |> should.equal(process.self())

  status.sys_state
  |> should.equal(api.ProcessRunning)
}

pub fn get_status_failure_test() {
  // This is a non-OTP process, so it should fail
  let pid = process.spawn(fn() { Nil })
  api.get_status(None, pid, 500)
  |> should.be_error
}

pub fn get_state_test() {
  let assert Ok(sub) = pantry.new()
  let assert Ok(pid) = process.subject_owner(sub)

  pantry.add_item(sub, "This actor has some state")

  let actual_state =
    pantry.list_items(sub)
    |> from()

  let inspected_state =
    api.get_state(None, pid, 500)
    |> should.be_ok()

  should.equal(inspected_state, actual_state)
}

pub fn get_state_failure_test() {
  // This is a non-OTP process, so it should fail
  let pid = process.spawn(fn() { Nil })
  api.get_state(None, pid, 500)
  |> should.be_error
}

// -------[ETS]

pub fn list_ets_tables_test() {
  // Create a test table
  let assert Ok(example) =
    table.build("test_table")
    |> table.privacy(table.Protected)
    |> table.write_concurrency(table.NoWriteConcurrency)
    |> table.read_concurrency(True)
    |> table.decentralized_counters(True)
    |> table.compression(False)
    |> table.set

  // Insert values
  example
  |> table.insert([#("hello", "joe")])

  let tables =
    api.list_ets_tables(None)
    |> should.be_ok

  let test_table =
    list.find(tables, fn(t) { t.name == atom.create("test_table") })
    |> should.be_ok

  test_table.table_type
  |> should.equal(atom.create("set"))

  test_table.size
  |> should.equal(1)

  test_table.memory
  |> dynamic.int()
  |> dynamic.classify()
  |> should.equal("Int")

  case test_table.owner {
    api.ProcessPrimitive(pid, _, _) -> {
      pid
      |> should.equal(process.self())
    }
    _ -> {
      should.fail()
    }
  }

  test_table.protection
  |> should.equal(atom.create("protected"))

  test_table.write_concurrency
  |> should.be_false()

  test_table.read_concurrency
  |> should.be_true()
}

pub fn get_ets_table_info_test() {
  // Create a test table
  let assert Ok(example) =
    table.build("test_table_ordered_set")
    |> table.privacy(table.Public)
    |> table.write_concurrency(table.WriteConcurrency)
    |> table.read_concurrency(True)
    |> table.decentralized_counters(True)
    |> table.compression(False)
    |> table.ordered_set

  // Insert values
  example
  |> table.insert([#("hello", "joe")])

  let info =
    api.get_ets_table_info(None, atom.create("test_table_ordered_set"))
    |> should.be_ok

  info.table_type
  |> should.equal(atom.create("ordered_set"))

  info.size
  |> should.equal(1)

  info.memory
  |> dynamic.int()
  |> dynamic.classify()
  |> should.equal("Int")

  case info.owner {
    api.ProcessPrimitive(pid, _, _) -> {
      pid
      |> should.equal(process.self())
    }
    _ -> {
      should.fail()
    }
  }

  info.protection
  |> should.equal(atom.create("public"))

  info.write_concurrency
  |> should.be_true()

  info.read_concurrency
  |> should.be_true()
}

pub fn get_ets_data_test() {
  // Create a test table
  let assert Ok(example) =
    table.build("test_table_data")
    |> table.privacy(table.Public)
    |> table.write_concurrency(table.WriteConcurrency)
    |> table.read_concurrency(True)
    |> table.decentralized_counters(True)
    |> table.compression(False)
    |> table.set

  // Insert values
  example
  |> table.insert([#("hello", "joe"), #("hello_2", "mike")])

  let table =
    api.get_ets_table_info(None, atom.create("test_table_data"))
    |> should.be_ok

  let data =
    api.get_ets_data(None, table)
    |> should.be_ok
    |> api.sort_table_data(0, api.Ascending)

  data
  |> should.equal(api.TableData(
    content: [
      [dynamic.string("hello"), dynamic.string("joe")],
      [dynamic.string("hello_2"), dynamic.string("mike")],
    ],
    max_length: 2,
  ))
}

// -------[PORTS]

pub fn get_port_list_test() {
  // Port process that sleeps for 10 seconds then exits
  let mock_port = open_port(#(atom.create("spawn"), "sleep 10"), [])
  let owner_primitive = api.ProcessPrimitive(process.self(), None, None)

  let ports = api.get_port_list(None)

  { list.length(ports) > 0 }
  |> should.be_true()

  let mock_port_item =
    list.find(ports, fn(p) { p.info.connected_process == owner_primitive })
    |> should.be_ok

  mock_port_item.port_id
  |> should.equal(mock_port)

  mock_port_item.info.memory
  |> dynamic.int()
  |> dynamic.classify()
  |> should.equal("Int")

  mock_port_item.info.queue_size
  |> dynamic.int()
  |> dynamic.classify()
  |> should.equal("Int")

  mock_port_item.info.os_pid
  |> should.be_some()
  |> dynamic.int()
  |> dynamic.classify()
  |> should.equal("Int")

  mock_port_item.info.input
  |> dynamic.int()
  |> dynamic.classify()
  |> should.equal("Int")

  mock_port_item.info.output
  |> dynamic.int()
  |> dynamic.classify()
  |> should.equal("Int")

  mock_port_item.info.registered_name
  |> should.be_none()

  mock_port_item.info.command_name
  |> should.equal("sleep 10")
}

pub fn get_port_details_test() {
  // Port process that sleeps for 10 seconds then exits
  let mock_port = open_port(#(atom.create("spawn"), "sleep 10"), [])
  link_port(mock_port)
  let owner_primitive = api.ProcessPrimitive(process.self(), None, None)
  let details =
    api.get_port_details(None, mock_port)
    |> should.be_ok

  details.links
  |> should.equal([owner_primitive])

  details.monitored_by
  |> should.equal([])

  details.monitors
  |> should.equal([])
}

// ------- [SYSTEM STATISTICS]

pub fn get_memory_statistics_test() {
  let stats =
    api.get_memory_statistics(None)
    |> should.be_ok()

  { stats.processes + stats.system }
  |> should.equal(stats.total)
}

pub fn get_word_size_test() {
  let word_size =
    api.get_word_size(None)
    |> should.be_ok()

  word_size
  |> should.equal(8)
}

pub fn get_system_info_test() {
  // let info =
  api.get_system_info(None)
  |> should.be_ok()
  // info.otp_release
  // |> should.equal("27")
}

// ------- TAG MANAGER GEN_SERVER

pub fn tag_test() {
  //  Should be able to add tag
  let assert Ok(sub) = pantry.new()
  let assert Ok(pid) = process.subject_owner(sub)

  // Ignore result because it might already be running
  // (would return already started error)
  let _ = api.start_tag_manager()

  api.add_tag(pid, "test tag")

  api.get_tag(pid)
  |> should.be_some
  |> should.equal("test tag")

  // Tag should be removed when process is killed
  pantry.close(sub)
  process.sleep(10)

  api.get_tag(pid)
  |> should.be_none
}

pub fn tag_info_test() {
  let assert Ok(sub) = pantry.new()
  let assert Ok(pid) = process.subject_owner(sub)

  // Ignore result because it might already be running
  // (would return already started error)
  let _ = api.start_tag_manager()
  api.add_tag(pid, "test tag")

  let info =
    api.get_process_info(None, pid)
    |> should.be_ok

  info.tag
  |> should.be_some
  |> should.equal("test tag")
}

@external(erlang, "erlang", "open_port")
fn open_port(
  command: #(atom.Atom, String),
  options: List(dynamic.Dynamic),
) -> port.Port

@external(erlang, "erlang", "link")
fn link_port(p: port.Port) -> Nil

@external(erlang, "gleam@function", "identity")
fn from(a: anything) -> dynamic.Dynamic
