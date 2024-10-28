import gleam/erlang/atom
import gleam/erlang/process
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result
import lustre
import lustre/attribute
import lustre/effect
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import spectator/internal/api
import spectator/internal/common
import spectator/internal/views/charts.{ChartSegment}
import spectator/internal/views/display

// MAIN ------------------------------------------------------------------------

pub fn app() {
  lustre.application(init, update, view)
}

// MODEL -----------------------------------------------------------------------

pub opaque type Model {
  Model(
    node: api.ErlangNode,
    params: common.Params,
    subject: Option(process.Subject(Msg)),
    refresh_interval: Int,
    memory_stats: Option(api.MemoryStatistics),
    memory_relative: Option(RelativeMemoryStatistics),
    system_info: Option(api.SystemInfo),
    system_limits: Option(RelativeSystemLimits),
    node_input: String,
    cookie_input: String,
    refresh_input: String,
  )
}

/// These are in percent and add up to 100
/// Except for `other_absolute` which is in bytes
/// and represents the missin 'other' part of system
/// which is not covered by the known categories
type RelativeMemoryStatistics {
  RelativeMemoryStatistics(
    processes: Float,
    code: Float,
    ets: Float,
    atom: Float,
    binary: Float,
    other: Float,
    other_absolute: Int,
  )
}

type RelativeSystemLimits {
  RelativeSystemLimits(processes: Float, ports: Float, atoms: Float, ets: Float)
}

fn init(params: common.Params) -> #(Model, effect.Effect(Msg)) {
  let node = api.node_from_params(params)
  let node_input = case node {
    Some(n) -> atom.to_string(n)
    None -> ""
  }
  let refresh_interval = common.get_refresh_interval(params)
  let cookie_input = common.get_param(params, "cookie") |> result.unwrap("")
  let initial_model =
    do_refresh(Model(
      node:,
      params: common.sanitize_params(params),
      subject: None,
      refresh_interval:,
      memory_stats: None,
      memory_relative: None,
      system_info: None,
      system_limits: None,
      node_input: node_input,
      cookie_input: cookie_input,
      refresh_input: int.to_string(refresh_interval),
    ))
  #(
    initial_model,
    common.emit_after(
      initial_model.refresh_interval,
      Refresh,
      option.None,
      CreatedSubject,
    ),
  )
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  Refresh
  CreatedSubject(process.Subject(Msg))
  NodeInputChanged(String)
  CookieInputChanged(String)
  RefreshInputChanged(String)
}

fn get_relative_memory_stats(input: api.MemoryStatistics) {
  let factor = 100.0 /. int.to_float(input.total)
  let processes = int.to_float(input.processes) *. factor
  let code = int.to_float(input.code) *. factor
  let ets = int.to_float(input.ets) *. factor
  let atom = int.to_float(input.atom) *. factor
  let binary = int.to_float(input.binary) *. factor
  let other = 100.0 -. processes -. code -. ets -. atom -. binary

  RelativeMemoryStatistics(
    processes:,
    code:,
    ets:,
    atom:,
    binary:,
    other:,
    other_absolute: input.system
      - input.atom
      - input.binary
      - input.code
      - input.ets,
  )
}

fn get_relative_system_limits(input: api.SystemInfo) {
  let processes =
    int.to_float(input.process_count)
    /. int.to_float(input.process_limit)
    *. 100.0
  let ports =
    int.to_float(input.port_count) /. int.to_float(input.port_limit) *. 100.0
  let atoms =
    int.to_float(input.atom_count) /. int.to_float(input.atom_limit) *. 100.0
  let ets =
    int.to_float(input.ets_count) /. int.to_float(input.ets_limit) *. 100.0

  RelativeSystemLimits(processes:, ports:, atoms:, ets:)
}

fn do_refresh(model: Model) -> Model {
  let memory_stats =
    api.get_memory_statistics(model.node)
    |> option.from_result
  let memory_relative = case memory_stats {
    None -> None
    Some(stats) -> Some(get_relative_memory_stats(stats))
  }
  let system_info =
    api.get_system_info(model.node)
    |> option.from_result
  let system_limits = case system_info {
    None -> None
    Some(info) -> Some(get_relative_system_limits(info))
  }
  Model(..model, memory_stats:, memory_relative:, system_info:, system_limits:)
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    NodeInputChanged(n) -> #(Model(..model, node_input: n), effect.none())
    CookieInputChanged(c) -> #(Model(..model, cookie_input: c), effect.none())
    RefreshInputChanged(r) -> #(Model(..model, refresh_input: r), effect.none())
    Refresh -> {
      #(
        do_refresh(model),
        common.emit_after(
          model.refresh_interval,
          Refresh,
          model.subject,
          CreatedSubject,
        ),
      )
    }
    CreatedSubject(subject) -> #(
      Model(..model, subject: Some(subject)),
      effect.none(),
    )
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  {
    use memory_stats <- option.then(model.memory_stats)
    use memory_relative <- option.then(model.memory_relative)
    use system_info <- option.then(model.system_info)
    use system_limits <- option.then(model.system_limits)

    Some(
      html.div([attribute.class("dashboard")], [
        split_section(
          [
            // Left
            html.h1([], [html.text("Inspect BEAM Nodes")]),
            html.div([attribute.class("info-container")], [
              // info_item("Currently Inspecting", case model.node {
              //   Some(n) -> atom.to_string(n)
              //   None -> "Self (The node running spectator)"
              // }),
              info_field(
                "Node",
                case model.node_input {
                  "" -> "Enter node name..."
                  n -> n
                },
                NodeInputChanged,
              ),
              info_field(
                "Cookie",
                case model.cookie_input {
                  "" -> "Enter cookie or leave empty"
                  n -> n
                },
                CookieInputChanged,
              ),
              info_field(
                "Refresh Interval (ms)",
                model.refresh_input,
                RefreshInputChanged,
              ),
              target_actions(model),
            ]),
            html.h1([], [html.text("System Information")]),
            html.div([attribute.class("info-container")], [
              info_item("Uptime", system_info.uptime),
              info_item("System Architecture", system_info.architecure),
              info_item("Runtime Version", system_info.erts_version),
              info_item("OTP Release", system_info.otp_release),
              info_item(
                "Schedulers",
                int.to_string(system_info.schedulers_online)
                  <> " / "
                  <> int.to_string(system_info.schedulers),
              ),
            ]),
          ],
          [
            // Right
            html.h1([], [html.text("System Limits")]),
            charts.meter(system_limits.processes),
            limit_label(
              "Processes",
              system_info.process_count,
              system_info.process_limit,
              system_limits.processes,
            ),
            charts.meter(system_limits.ports),
            limit_label(
              "Ports",
              system_info.port_count,
              system_info.port_limit,
              system_limits.ports,
            ),
            charts.meter(system_limits.atoms),
            limit_label(
              "Atoms",
              system_info.atom_count,
              system_info.atom_limit,
              system_limits.atoms,
            ),
            charts.meter(system_limits.ets),
            limit_label(
              "ETS",
              system_info.ets_count,
              system_info.ets_limit,
              system_limits.ets,
            ),
          ],
        ),
        render_memory_section(memory_stats, memory_relative),
      ]),
    )
  }
  |> option.unwrap(
    html.div([attribute.class("component-error")], [
      html.text("Could not fetch dashboard data"),
    ]),
  )
}

fn limit_label(label: String, value: Int, limit: Int, percent: Float) {
  html.div([attribute.class("limit-item")], [
    html.text(
      label
      <> ": "
      <> int.to_string(value)
      <> " / "
      <> int.to_string(limit)
      <> " ("
      <> common.format_percentage(percent)
      <> ")",
    ),
  ])
}

fn info_item(label, value) {
  html.div([attribute.class("info-item")], [
    html.div([attribute.class("info-label")], [html.text(label)]),
    html.div([attribute.class("info-value")], [html.text(value)]),
  ])
}

fn info_field(label, default_value, handler) {
  html.div([attribute.class("info-item")], [
    html.label([attribute.class("info-label")], [html.text(label)]),
    html.input([
      event.on_input(handler),
      attribute.class("info-value"),
      attribute.placeholder(default_value),
    ]),
  ])
}

fn target_actions(model: Model) {
  html.div([attribute.class("info-item")], [
    html.a([attribute.class("button"), attribute.href("/dashboard")], [
      html.text("Reset to Self"),
    ]),
    html.a(
      [
        attribute.class("button"),
        attribute.href(
          "/dashboard"
          <> []
          |> common.add_param("node", model.node_input)
          |> common.add_param("cookie", model.cookie_input)
          |> common.add_param("refresh", model.refresh_input)
          |> common.encode_params(),
        ),
      ],
      [html.text("Inspect Target")],
    ),
  ])
}

fn split_section(left, right) {
  html.div([attribute.class("split")], [
    html.div([attribute.class("split-left")], left),
    html.div([attribute.class("split-right")], right),
  ])
}

fn render_memory_section(
  memory_stats: api.MemoryStatistics,
  memory_relative: RelativeMemoryStatistics,
) {
  html.section([], [
    html.h1([], [html.text("Memory Usage")]),
    charts.column_chart([
      ChartSegment(
        common.colour_process,
        "Processes",
        memory_relative.processes,
      ),
      ChartSegment(common.colour_code, "Code ", memory_relative.code),
      ChartSegment(common.colour_ets, "ETS", memory_relative.ets),
      ChartSegment(common.colour_atom, "Atoms", memory_relative.atom),
      ChartSegment(common.colour_binary, "Binaries", memory_relative.binary),
      ChartSegment(common.colour_other, "Other", memory_relative.other),
    ]),
    html.div([attribute.class("memory-breakdown")], [
      charts.legend_item(
        "Total",
        "var(--background)",
        display.storage(memory_stats.total),
      ),
      charts.legend_item(
        "Processes",
        common.colour_process,
        display.storage(memory_stats.processes),
      ),
      charts.legend_item(
        "Code",
        common.colour_code,
        display.storage(memory_stats.code),
      ),
      charts.legend_item(
        "ETS",
        common.colour_ets,
        display.storage(memory_stats.ets),
      ),
      charts.legend_item(
        "Atoms",
        common.colour_atom,
        display.storage(memory_stats.atom),
      ),
      charts.legend_item(
        "Binaries",
        common.colour_binary,
        display.storage(memory_stats.binary),
      ),
      charts.legend_item(
        "Other",
        common.colour_other,
        display.storage(memory_relative.other_absolute),
      ),
    ]),
  ])
}
