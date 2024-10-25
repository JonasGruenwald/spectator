import gleam/float
import gleam/list
import lustre/attribute.{attribute}
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/svg
import spectator/internal/common

const column_chart_height = "100"

const meter_height = "25"

pub type ChartSegment {
  ChartSegment(colour: String, label: String, value: Float)
}

fn map_segments_with_offset(
  input: List(ChartSegment),
  acc: List(Element(a)),
  offset: Float,
) {
  case input {
    [] -> list.reverse(acc)
    [s, ..rest] -> {
      map_segments_with_offset(
        rest,
        [
          // Doing this because svg.rect doesn't allow setting children,
          // and <title/> needs to be a child of the element
          element.namespaced(
            "http://www.w3.org/2000/svg",
            "rect",
            [
              attribute.title(
                s.label <> " " <> common.format_percentage(s.value),
              ),
              attribute.style([
                #("x", float.to_string(offset) <> "px"),
                #("y", "0px"),
                #("height", column_chart_height <> "px"),
                #("width", float.to_string(s.value) <> "px"),
                #("fill", s.colour),
              ]),
            ],
            [
              svg.title([], [
                html.text(s.label <> " " <> common.format_percentage(s.value)),
              ]),
            ],
          ),
          ..acc
        ],
        offset +. s.value,
      )
    }
  }
}

/// This column chart assumes that the sum of the values is 100
/// (percentage based data)
pub fn column_chart(segments: List(ChartSegment)) {
  html.svg(
    [
      attribute.class("column-chart"),
      attribute("viewBox", "0 0 100 " <> column_chart_height),
      attribute("height", column_chart_height <> "px"),
      attribute("width", "100%"),
      attribute("preserveAspectRatio", "none"),
    ],
    map_segments_with_offset(segments, [], 0.0),
  )
}

pub fn legend_item(label: String, color: String, value: Element(a)) {
  html.div([attribute.class("legend-item")], [
    html.div([attribute.class("legend-colour")], [
      html.div([attribute.style([#("background-color", color)])], []),
    ]),
    html.div([], [
      html.div([attribute.class("legend-label")], [html.text(label)]),
      value,
    ]),
  ])
}

/// This meter assumes that the max value is 100
/// (percentage based data)
pub fn meter(value: Float) {
  html.svg(
    [
      attribute.class("meter-chart"),
      attribute("viewBox", "0 0 100 " <> meter_height),
      attribute("height", meter_height <> "px"),
      attribute("width", "100%"),
      attribute("preserveAspectRatio", "none"),
    ],
    [
      svg.rect([
        attribute.style([
          #("x", "0px"),
          #("y", "0px"),
          #("height", meter_height <> "px"),
          #("width", float.to_string(value) <> "px"),
          #("fill", "var(--meter-bar)"),
        ]),
      ]),
    ],
  )
}
