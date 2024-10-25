import gleam/erlang/atom
import gleam/erlang/process
import gleam/int
import spectator
import spectator/internal/api
import utils/pantry

fn keep_adding_to_table(ets_table: atom.Atom, count: Int) {
  api.ets_insert(ets_table, [#(count, "Row number " <> int.to_string(count))])
  process.sleep(1000)
  keep_adding_to_table(ets_table, count + 1)
}

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
  let pid = process.subject_owner(sub2)
  // register the actor under a name
  register(atom.create_from_string("registered_actor"), pid)

  monitor_by_name(
    atom.create_from_string("process"),
    atom.create_from_string("registered_actor"),
  )

  // Create an ETS table

  let assert Ok(table1) =
    api.new_ets_table(atom.create_from_string("test_table"))
  api.ets_insert(table1, [#(1, "one"), #(2, "two"), #(3, "three"), #(4, "four")])

  let assert Ok(table2) =
    api.new_ets_table(atom.create_from_string(
      "https://table-is-not-urlsafe/lol.com?query=1",
    ))
  api.ets_insert(table2, [#("Wibble", "Wobble")])

  let assert Ok(table3) =
    api.new_ets_table(atom.create_from_string("Wibble Wooble"))
  process.start(fn() { keep_adding_to_table(table3, 1) }, False)
  |> spectator.tag("Rogue ETS Process")

  // Start another OTP actor and tag it for spectator
  // let assert Ok(_browser) =
  //   chrobot.launch()
  //   |> spectator.tag_result("Chrobot Browser")

  // let assert Ok(_) = chrobot.open(browser, "http://127.0.0.1:3000/processes/", 5_000)
  // Sleep on the main process so the program doesn't exit
  spectator.tag(process.self(), "Playground Main")
  process.sleep_forever()
}

@external(erlang, "erlang", "register")
fn register(name: atom.Atom, pid: process.Pid) -> Nil

@external(erlang, "erlang", "monitor")
fn monitor_by_name(resource: atom.Atom, name: atom.Atom) -> Nil
