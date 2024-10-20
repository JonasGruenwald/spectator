import gleam/dynamic
import gleam/erlang/atom
import gleam/erlang/process
import gleam/list
import gleam/option
import gleeunit/should
import spectator/internal/api
import utils/pantry

pub fn list_processes_test() {
  // Start a process
  let assert Ok(sub) = pantry.new()
  let pid = process.subject_owner(sub)

  // Check that the process is in the list of processes
  api.list_processes()
  |> list.find(fn(p) { p == pid })
  |> should.be_ok
}

pub fn get_status_test() {
  // Start an OTP actor
  let assert Ok(sub) = pantry.new()
  let pid = process.subject_owner(sub)

  // Retrieve the status for that actor
  let status =
    api.get_status(pid, 500)
    |> should.be_ok

  let assert Ok(actor_module_name) = atom.from_string("gleam@otp@actor")

  status.module
  |> should.equal(actor_module_name)

  status.pid
  |> should.equal(pid)

  status.parent
  |> should.equal(process.self())

  status.sys_state
  |> should.equal(api.Running)
}

pub fn get_status_failure_test() {
  // This is a non-OTP process, so it should fail
  let pid = process.start(fn() { Nil }, True)
  api.get_status(pid, 500)
  |> should.be_error
}

pub fn get_state_test() {
  let assert Ok(sub) = pantry.new()
  let pid = process.subject_owner(sub)

  pantry.add_item(sub, "This actor has some state")

  let actual_state =
    pantry.list_items(sub)
    |> dynamic.from()

  let inspected_state =
    api.get_state(pid, 500)
    |> should.be_ok()

  should.equal(inspected_state, actual_state)
}

pub fn get_state_failure_test() {
  // This is a non-OTP process, so it should fail
  let pid = process.start(fn() { Nil }, True)
  api.get_state(pid, 500)
  |> should.be_error
}

pub fn get_info_test() {
  let info =
    api.get_info(process.self())
    |> should.be_ok

  info.current_function.0
  |> should.equal(atom.create_from_string("spectator_ffi"))

  info.current_function.1
  |> should.equal(atom.create_from_string("get_info"))

  info.current_function.2
  |> should.equal(1)

  info.initial_call.0
  |> should.equal(atom.create_from_string("erlang"))

  info.initial_call.1
  |> should.equal(atom.create_from_string("apply"))

  info.initial_call.2
  |> should.equal(2)

  info.registered_name
  |> should.equal(option.None)

  { info.memory > 0 }
  |> should.be_true

  { info.message_queue_len >= 0 }
  |> should.be_true

  { info.reductions >= 0 }
  |> should.be_true

  info.tag
  |> should.be_none
}

pub fn get_details_test() {
  let details =
    api.get_details(process.self())
    |> should.be_ok

  details.trap_exit
  |> should.be_false
}

pub fn tag_test() {
  //  Should be able to add tag
  let assert Ok(sub) = pantry.new()
  let pid = process.subject_owner(sub)

  api.start_tag_manager()
  |> should.be_ok
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

pub fn ets_test() {
  let table_name = atom.create_from_string("test_table")
  api.new_ets_table(table_name)
  |> should.be_ok

  api.ets_insert(table_name, [#("hello", "joe")])

  api.list_ets_tables()
  |> list.find(fn(t) {
    case t {
      api.NamedTable(name) if name == table_name -> True
      _ -> False
    }
  })
  |> should.be_ok

  api.get_ets_data(table_name)
  |> should.be_ok
  |> list.first
  |> should.be_ok
  |> list.first
  |> should.be_ok
  |> should.equal(#(dynamic.from("hello"), dynamic.from("joe")))
}
