import gleam/erlang
import simplifile

pub const message_queue_threshold = 10

pub const refresh_interval = 500

pub fn static_file(name: String) {
  let assert Ok(priv) = erlang.priv_directory("spectator")
  let assert Ok(data) = simplifile.read(priv <> "/" <> name)
  data
}