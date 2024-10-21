import gleam/erlang/atom
import gleam/erlang/process
import gleam/int
import gleam/io
import spectator
import spectator/internal/api
import utils/pantry

fn keep_adding_to_table(ets_table: atom.Atom, count: Int) {
  io.debug("Putting another shrimp on the barbie")
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

  // Start another OTP actor and tag it for spectator
  // let assert Ok(browser) = chrobot.launch_window()
  // |> spectator.tag_result("Chrobot Browser")

  // let assert Ok(_) = chrobot.open(browser, "http://127.0.0.1:3000/processes/", 5_000)
  // Sleep on the main process so the program doesn't exit
  process.sleep_forever()
}
