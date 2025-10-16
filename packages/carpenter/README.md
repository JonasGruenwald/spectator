# carpenter 🔨

[![Package Version](https://img.shields.io/hexpm/v/carpenter)](https://hex.pm/packages/carpenter)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/carpenter/)

Bindings for Erlang's [ETS tables](https://www.erlang.org/doc/man/ets.html).
Forked and updated from [gts](https://github.com/Lunarmagpie/gts).
Carpenter implements support for `set` and `ordered_set`, but does not
supports `bag` or `ordered_bag` ETS table as of now.

If you aren't familiar with ETS tables,
[this](https://elixirschool.com/en/lessons/storage/ets) is a good introduction.

## Quick start

```gleam
import carpenter/table

pub fn main() {
  // Set up and configure an ETS table
  let assert Ok(example) =
    table.build("table_name")
    |> table.privacy(table.Private)
    |> table.write_concurrency(table.AutoWriteConcurrency)
    |> table.read_concurrency(True)
    |> table.decentralized_counters(True)
    |> table.compression(False)
    |> table.set

  // Insert a value
  example
  |> table.insert([#("hello", "world")])

  // Retrieve a key-value tuple
  example
  |> table.lookup("hello")
  |> echo

  // Delete an object
  example
  |> table.delete("hello")

  // Delete a table
  example
  |> table.drop
}
```

## Installation

This package is available on hex:

```sh
gleam add carpenter
```

Its documentation can be found at <https://hexdocs.pm/carpenter>.
