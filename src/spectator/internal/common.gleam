import gleam/erlang
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string_builder
import gleam/uri
import lustre/effect
import lustre/server_component
import simplifile

pub const message_queue_threshold = 10

pub const refresh_interval_default = 1000

pub const colour_process = "#EF5976"

pub const colour_code = "#FDA870"

pub const colour_ets = "#ECE27C"

pub const colour_atom = "#95EA8C"

pub const colour_binary = "#91D0DA"

pub const colour_other = "#B498F6"

pub type Params =
  List(#(String, String))

pub fn encode_params(params: Params) -> String {
  case params {
    [] -> ""
    _ -> "?" <> uri.query_to_string(params)
  }
}

pub fn add_param(params: Params, key: String, value: String) -> Params {
  [#(key, value), ..params]
}

pub fn get_param(params: Params, key: String) -> Result(String, Nil) {
  list.find_map(params, fn(p) {
    case p {
      #(k, v) if k == key -> Ok(v)
      _ -> Error(Nil)
    }
  })
}

pub fn sanitize_params(params: Params) -> Params {
  list.filter_map(params, fn(p) {
    case p {
      #("node", _) -> Ok(p)
      #("cookie", _) -> Ok(p)
      #("refresh", _) -> Ok(p)
      _ -> Error(Nil)
    }
  })
}

pub fn get_refresh_interval(params: Params) -> Int {
  case get_param(params, "refresh") {
    Ok(rate) ->
      case int.parse(rate) {
        Ok(r) -> r
        Error(_) -> refresh_interval_default
      }
    Error(_) -> refresh_interval_default
  }
}

@external(erlang, "spectator_ffi", "truncate_float")
pub fn truncate_float(value: Float) -> String

pub fn format_percentage(value: Float) -> String {
  truncate_float(value) <> "%"
}

pub fn static_file(name: String) {
  let assert Ok(priv) = erlang.priv_directory("spectator")
  let assert Ok(data) = simplifile.read(priv <> "/" <> name)
  data
}

pub fn emit_after(
  delay: Int,
  msg: a,
  subject: Option(process.Subject(a)),
  subject_created_message: fn(process.Subject(a)) -> a,
) -> effect.Effect(a) {
  case subject {
    Some(self) -> {
      use _ <- effect.from
      let _ = process.send_after(self, delay, msg)
      Nil
    }
    None -> {
      use dispatch, subject <- server_component.select
      let selector =
        process.new_selector() |> process.selecting(subject, fn(msg) { msg })
      let _ = process.send_after(subject, delay, msg)
      dispatch(subject_created_message(subject))
      selector
    }
  }
}