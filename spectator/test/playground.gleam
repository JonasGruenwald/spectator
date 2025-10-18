import carpenter/table
import gleam/erlang/atom
import gleam/erlang/process
import spectator
import utils/pantry

pub fn main() {
  // Start the spectator service
  let assert Ok(_) = spectator.start()

  // Start an OTP actor
  let assert Ok(sub) = pantry.new()

  // Tag the actor with a name for easier identification in the spectator UI
  // Note: this only works because we called spectator.start before!
  sub
  |> spectator.tag_subject("Pantry Actor")

  // Add some state to the actor
  pantry.add_item(sub, "This actor has some state")
  pantry.add_item(sub, "Another item in the state of this actor")
  pantry.add_item(sub, "And some more state I've put into this demo actor")

  // Start another OTP actor
  let assert Ok(sub2) =
    pantry.new()
    |> spectator.tag_result("Registered Pantry Actor")
  pantry.add_item(sub2, "Helllo")
  let assert Ok(pid) = process.subject_owner(sub2)
  // register the actor under a name
  register(atom.create("registered_actor"), pid)

  monitor_by_name(atom.create("process"), atom.create("registered_actor"))

  //  Create some tables
  let assert Ok(t1) =
    table.build("https://table-is-not-urlsafe/lol.com?query=1")
    |> table.set

  t1
  |> table.insert([#("hello", "joe")])

  // let assert Ok(_) = chrobot.open(browser, "http://127.0.0.1:3000/processes/", 5_000)
  // Sleep on the main process so the program doesn't exit
  spectator.tag(process.self(), "Playground Main")
  process.sleep_forever()
}

@external(erlang, "erlang", "register")
fn register(name: atom.Atom, pid: process.Pid) -> Nil

@external(erlang, "erlang", "monitor")
fn monitor_by_name(resource: atom.Atom, name: atom.Atom) -> Nil
