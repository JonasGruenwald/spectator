import gleam/io
import spectator/internal/api

/// Start the spectator application.
pub fn main() {
  io.debug("Hello, joe!")
}

/// Tag the current process with a name that can be used to identify it in the spectator UI.
/// You must call this function from **within** the process you want to tag.
pub fn tag(name: String) -> Nil {
  api.add_tag(name)
}
