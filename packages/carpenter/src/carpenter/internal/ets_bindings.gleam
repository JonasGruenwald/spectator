//// [Erlang Documentation](https://www.erlang.org/doc/apps/stdlib/ets.html)

import gleam/dynamic
import gleam/erlang/atom

/// Returns a list of all tables at the node. Named tables are specified by
/// their names, unnamed tables are specified by their table identifiers.
@external(erlang, "ets", "all")
pub fn all() -> List(atom.Atom)

/// Deletes the entire table.
@external(erlang, "ets", "delete")
pub fn drop(table: atom.Atom) -> Bool

/// Deletes all objects with key from table. This function succeeds
/// even if no objects with key exist.
@external(erlang, "ets", "delete")
pub fn delete_key(table: atom.Atom, key: k) -> Bool

/// Delete all objects in the ETS table. The operation is guaranteed to be
/// atomic and isolated.
@external(erlang, "ets", "delete_all_objects")
pub fn delete_all_objects(table: atom.Atom) -> Bool

/// Delete the exact object from the ETS table, leaving objects with
/// the same key but other differences (useful for type bag). In a
/// `duplicate_bag` table, all instances of the object are deleted.
@external(erlang, "ets", "delete_object")
pub fn delete_object(table: atom.Atom, object: #(k, v)) -> Bool

/// Inserts all of the objects in list into table.
@external(erlang, "ets", "insert")
pub fn insert(table: atom.Atom, tuple: List(#(k, v))) -> Bool

/// Same as `insert` except that instead of overwriting objects with the
/// same key (for `set` or `ordered_set`) or adding more objects with keys
/// already existing in the table (for `bag` and `duplicate_bag`), `False`
/// is returned.
@external(erlang, "ets", "insert_new")
pub fn insert_new(table: atom.Atom, tuple: List(#(k, v))) -> Bool

/// Returns a list of all objects with key in table.
@external(erlang, "ets", "lookup")
pub fn lookup(table: atom.Atom, key: k) -> List(#(k, v))

/// Make process `pid` the new owner of the table. If successful, message
/// `{'ETS-TRANSFER',Table,FromPid,GiftData}` is sent to the new owner.
@external(erlang, "ets", "give_away")
pub fn give_away(table: atom.Atom, pid: pid, gift_data: any) -> Bool

/// Creates a new table and returns a table identifier that can be used in
/// subsequent operations. The table identifier can be sent to other processes
/// so that a table can be shared between different processes within a node.
@external(erlang, "carpenter_ffi", "new_table")
pub fn new_table(
  name: atom.Atom,
  props: List(dynamic.Dynamic),
) -> Result(atom.Atom, Nil)

/// Works like `lookup`, but does not return the objects. Returns `True` if one
/// or more elements in the table has key, otherwise `False`.
@external(erlang, "ets", "member")
pub fn member(table: atom.Atom, key: k) -> Bool

/// Returns and removes a list of all objects with key in table.
@external(erlang, "ets", "take")
pub fn take(table: atom.Atom, key: k) -> List(#(k, v))
