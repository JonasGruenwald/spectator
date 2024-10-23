import gleam/erlang/process
import gleam/float
import gleam/int
import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/string
import lustre
import lustre/attribute
import lustre/effect
import lustre/element.{type Element}
import lustre/element/html
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
    subject: Option(process.Subject(Msg)),
    memory_stats: Option(api.MemoryStatistics),
    memory_relative: Option(RelativeMemoryStatistics),
  )
}

/// Ther e are in percent and add up to 100
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

fn init(_params: common.Params) -> #(Model, effect.Effect(Msg)) {
  #(
    do_refresh(Model(subject: None, memory_stats: None, memory_relative: None)),
    common.emit_after(
      common.refresh_interval,
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

fn do_refresh(model: Model) -> Model {
  let memory_stats = api.get_memory_statistics() |> option.from_result
  let memory_relative = case memory_stats {
    None -> None
    Some(stats) -> Some(get_relative_memory_stats(stats))
  }
  Model(..model, memory_stats:, memory_relative:)
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    Refresh -> {
      #(
        do_refresh(model),
        common.emit_after(
          common.refresh_interval,
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

    Some(
      html.div([attribute.class("dashboard")], [
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
