import gleam/float
import gleam/int
import gleam/list
import gleam/string
import lustre/attribute.{attribute}
import lustre/element.{type Element}
import lustre/element/html
import lustre/element/svg

const column_chart_height = "100"

pub type ChartSegment {
  ChartSegment(colour: String, label: String, value: Float)
}

fn format_percentage(value: Float) -> String {
  case
    float.to_string(value)
    |> string.split(".")
  {
    [whole, decimal] -> {
      let decimal = string.slice(decimal, 0, 2)
      whole <> "." <> decimal <> "%"
    }
    _ -> "0%"
  }
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
              attribute.title(s.label <> " " <> format_percentage(s.value)),
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
                html.text(s.label <> " " <> format_percentage(s.value)),
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
