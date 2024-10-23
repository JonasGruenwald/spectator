import gleam/float
import gleam/int
import gleam/list
import lustre/attribute.{attribute}
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/svg

const column_chart_height = "100"

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
          svg.rect([
            attribute.style([
              #("x", float.to_string(offset)),
              #("y", "0"),
              #("height", column_chart_height),
              #("width", float.to_string(s.value)),
              #("fill", s.colour),
            ]),
          ]),
          ..acc
        ],
        offset +. s.value,
      )
    }
  }
}

pub fn column_chart(value_total: Int, segments: List(ChartSegment)) {
  html.svg(
    [
      attribute.class("column-chart"),
      attribute(
        "viewBox",
        "0 0 " <> int.to_string(value_total) <> " " <> column_chart_height,
      ),
      attribute("height", column_chart_height <> "px"),
      attribute("width", "100%"),
      attribute("preserveAspectRatio", "none"),
    ],
    map_segments_with_offset(segments, [], 0.0),
  )
}
