## <img width=16 src="https://raw.githubusercontent.com/JonasGruenwald/spectator/main/spectator/priv/lucy_spectator.svg"> Spectator

[![Package Version](https://img.shields.io/hexpm/v/spectator)](https://hex.pm/packages/spectator)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/spectator/)

Spectator is a BEAM observer tool written in Gleam, that plays well with gleam_otp processes.

![](https://raw.githubusercontent.com/JonasGruenwald/spectator/refs/heads/main/assets/screenshot.png)

## Features

This is a work in progress, so far it has the following features:

- Show processes in a sortable table
- Tag individual processes for easy identification
- Show process details
- Show OTP process state
- Suspend / resume OTP processes
- List ETS tables
- View content of ETS tables
- List of active ports
- Clickable links between resources
- Dashboard with basic statistics
- Inspect other BEAM nodes

## Use Spectator in Development

You can install spectator as a dependency in your project, and run it as part of your application to inspect your app in development.

Install as a dependency

```sh
gleam add spectator
```

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

## Use Spectator in Production

You can use spectator to inspect the BEAM node running your deployed application by running spectator as a standalone app, and connecting it to the erlang node your application is running on via distribution.

The standalone spectator application is available as a docker image via `ghcr.io/jonasgruenwald/spectator`

First, ensure your application has an Erlang name and cookie set, for example by setting the `ERL_FLAGS` environment variable before your Gleam application is started:

```sh
ERL_FLAGS="-sname myapp -setcookie mycookie"
```

If your application is running in a docker container, put both your applications container, and the container running spectator on the same docker network, and ensure that your applications container has its hostname set to a value you know.

Start spectator, and click 'Change Target' in the top right corner of the application.

Enter the details of the node you wish to inspect, the name should be the name you set via `-sname`, followed by `@`, and then the hostname.

Finally, click 'Connect', to inspect the your application node.

Please note that all connection information, including the cookie, is stored plainly in the URL query parameters of the spectator application.

## Considerations

Please be aware of the following implications of running spectator:

- **Spectator may slow down your system**  
  All displayed processes are probed in the configured interval using Erlang's `process_info/2` function which puts a temporary lock on the process being infoed. If the process is handling a lot of messages, this may have performance implications for the system
- **Spectator will send system messages to selected processes**  
  In order to get a selected processes OTP state, spectator needs to send it system messages. If you select a process that is not handling these messages properly, spectator may fill up its message queue, as it is sending a new message every tick in the configured interval. If the processes message queue is over a certain size, spectator will stop sending new messages, however the process may never handle the already queued up messages
- **Spectator will create atoms dynamically**  
  When you choose to connect to other Erlang nodes, spectator needs to convert the node name and cookie you provide into atoms. Therefore it is possible to exhaust the memory of the BEAM instance running spectator using its user interface, as atoms are never garbage collected.

---

Further documentation can be found at <https://hexdocs.pm/spectator>.
