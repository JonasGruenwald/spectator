import gleam/erlang/process
import spectator/wibble/wobble

pub fn main() {
  wobble.hello()
}

pub fn tag(start_response: Result(process.Subject(b), e)) {
  //  return input unchanged
  start_response
}
