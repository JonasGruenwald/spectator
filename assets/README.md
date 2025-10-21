# Assets

This module is for turning assets into Gleam modules, so that spectator can run without relying on a priv directory, which is necessary for the escript version of the standalone app.

To generate modules from the assets folder run

```sh
gleam run -m embeds/files
```
