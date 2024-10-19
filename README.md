## <img width=32 src="https://raw.githubusercontent.com/JonasGruenwald/spectator/main/priv/lucy_spectator.svg"> Spectator
[![Package Version](https://img.shields.io/hexpm/v/spectator)](https://hex.pm/packages/spectator)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/spectator/)

Spectator is a BEAM observer tool written in Gleam, that plays well with gleam_otp processes.

![](/priv/screenshot.png)

## Installation

```sh
gleam add spectator@1
```

## Usage

> 🦔 **Spectator is a tool intended to be used for debugging and inspecting applications in development, you should not include spectator in your production application**

Call `spectator.start()` in your application, to start the spectator service and WebUI.

In order to make it easier to identify your Gleam processes in the process list, you can tag them with a friendly name with `spectator.tag`, `spectator.tag_subject` or `spectator.tag_result` **after** starting the spectator service.

## Example

```gleam
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

  // Sleep on the main process so the program doesn't exit
  process.sleep_forever()
}
```

Further documentation can be found at <https://hexdocs.pm/spectator>.
