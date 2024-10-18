import gleam/erlang
import simplifile

pub fn static_file(name: String) {
  let assert Ok(priv) = erlang.priv_directory("spectator")
  let assert Ok(data) = simplifile.read(priv <> "/" <> name)
  data
}
