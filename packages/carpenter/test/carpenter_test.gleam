import carpenter/table
import gleam/result
import gleeunit

pub fn main() {
  gleeunit.main()
}

pub fn set_insert_test() {
  let assert Ok(table) = table.build("set_insert_test") |> table.set
  assert table.insert(table, [#("hello", "world")]) == Nil
  assert table.lookup(table, "hello") == [#("hello", "world")]
}

pub fn set_insert_new_test() {
  let assert Ok(table) = table.build("set_insert_new_test") |> table.set
  assert table.insert(table, [#(1, 2), #(2, 3)]) == Nil
  assert !table.insert_new(table, [#(3, 4), #(1, 3)])
  assert table.insert_new(table, [#(3, 4), #(4, 5)])
}

pub fn set_delete_test() {
  let assert Ok(table) = table.build("delete_test") |> table.set
  assert table.insert(table, [#(1, 2)]) == Nil
  assert table.delete(table, 1) == Nil
  assert table.lookup(table, 1) == []
}

pub fn set_delete_all_test() {
  let assert Ok(table) = table.build("delete_all_test") |> table.set
  assert table.insert(table, [#(1, 2), #(2, 3)]) == Nil
  assert table.delete_all(table) == Nil
  assert table.lookup(table, 1) == []
  assert table.lookup(table, 2) == []
}

pub fn set_delete_object_test() {
  let assert Ok(table) = table.build("delete_obj_test") |> table.set
  assert table.insert(table, [#(1, 2)]) == Nil
  assert table.delete_object(table, #(1, 2)) == Nil
  assert !table.contains(table, 1)
}

pub fn ordered_set_test() {
  let assert Ok(table) = table.build("ordered_set_test") |> table.ordered_set
  assert table.insert(table, [#(1, 2), #(2, 3)]) == Nil
  assert table.lookup(table, 1) == [#(1, 2)]
}

pub fn drop_test() {
  let assert Ok(table) = table.build("drop_test") |> table.set
  assert table.build("drop_test") |> table.set() == Error(Nil)
  assert table.drop(table) == Nil
  assert table.build("drop_test")
    |> table.set()
    |> result.is_ok
}

pub fn contains_test() {
  let assert Ok(table) = table.build("contains_test") |> table.set()
  assert table.insert(table, [#(1, 2)]) == Nil
  assert table.contains(table, 1)
  assert !table.contains(table, 2)
}

pub fn ref_test() {
  let assert Ok(table) = table.ref("contains_test")
  assert table.contains(table, 1)
}

pub fn take_test() {
  let assert Ok(table) = table.build("take_test") |> table.set()
  assert table.insert(table, [#(1, 2)]) == Nil
  assert table.contains(table, 1)
  assert table.take(table, 1) == [#(1, 2)]
  assert !table.contains(table, 1)
}
