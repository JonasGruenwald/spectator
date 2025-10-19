import gleam/erlang/application
import gleam/erlang/process
import gleam/otp/actor
import gleam/otp/static_supervisor as supervisor
import spectator

pub fn start(_application, _start_type: application.StartType) {
  spectator.start_on("0.0.0.0", 3000)
  |> to_application_start_result
}

fn to_application_start_result(
  result: Result(actor.Started(supervisor.Supervisor), actor.StartError),
) {
  case result {
    Ok(actor.Started(pid:, ..)) -> {
      Ok(pid)
    }
    Error(start_error) -> {
      Error(start_error)
    }
  }
}

/// We are using the erlang implicit application start behaviour
/// so this function is just here so that `gleam run` works.
pub fn main() -> Nil {
  process.sleep_forever()
}
