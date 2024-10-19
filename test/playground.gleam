import gleam/erlang/process
import gleam/io
import spectator
import spectator/internal/api
import utils/pantry

pub fn main() {
  spectator.start()

  // Start an OTP actor
  let assert Ok(sub) =
    pantry.new()
    |> spectator.tag_result("Pantry Actor")

  let pid = process.subject_owner(sub)

  pantry.add_item(sub, "This actor has some state")
  pantry.add_item(sub, "Another item in the state of this actor")
  pantry.add_item(sub, "And some more state I've put into this demo actor")

  io.debug(pid)

  let tag = api.get_tag(pid)

  process.sleep_forever()
}
