//// Props are options list used in Erlang API. Most of the time, they take the
//// form of options, that can be either atoms, or tuples of atoms (or other
//// data, like numbers, etc.). That module helps dealing with such data
//// structures while providing simple primitives to work with `Option(a)` or
//// `Bool`.
////
//// ```erl
//// % Typically in Erlang, starting an ETS table looks like this.
//// ets:new(mytable, [set, compressed, {write_concurrency, true}]).
//// ```

import gleam/bool
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/list
import gleam/option.{type Option}

pub type Props =
  List(decode.Dynamic)

/// Append a prop to the props, if the value has been set. That function is made
/// to be used with a builder that set fields as optional.
pub fn append(
  props: Props,
  prop: Option(a),
  mapper: fn(a) -> decode.Dynamic,
) -> Props {
  prop
  |> option.map(mapper)
  |> option.map(list.prepend(props, _))
  |> option.unwrap(props)
}

/// Append a prop to the props, if the value is true. That function is made
/// to be used with a builder that set fields as false by default.
pub fn append_if(
  props: Props,
  prop: Bool,
  mapper: fn() -> decode.Dynamic,
) -> Props {
  use <- bool.guard(when: !prop, return: props)
  [mapper(), ..props]
}

/// Helper to generate an atom. Instead of continuously generating it, and
/// coercing it as `Dynamic`, `atom` directly generate a dynamic atom.
pub fn atom(name: String) -> decode.Dynamic {
  let value = atom.create(name)
  coerce(value)
}

/// Helper to generate a pair of atoms, directly from strings. Simplify code
/// and easy generating of atoms.
pub fn pair(prop: String, value: a) -> decode.Dynamic {
  let prop = atom(prop)
  let value = coerce(value)
  coerce(#(prop, value))
}

/// Used to replace `dynamic.from`, which is now deprecated. Because we're
/// working with Erlang functions that could be highly dynamic, it's often
/// necessary to convert our data structure to `Dynamic`.
@external(erlang, "carpenter_ffi", "coerce")
fn coerce(a: a) -> decode.Dynamic
