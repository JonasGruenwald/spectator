//// Generic table rendering view functions

import gleam/bool
import gleam/list
import lustre/attribute
import lustre/element
import lustre/element/html
import lustre/event
import spectator/internal/api

pub fn heading(
  name: String,
  title: String,
  sort_criteria: s,
  current_sort_criteria: s,
  current_sort_direction: api.SortDirection,
  handle_heading_click: fn(s) -> a,
  align_right align_right: Bool,
) {
  html.th(
    [
      attribute.title(title),
      event.on_click(handle_heading_click(sort_criteria)),
      bool.guard(
        when: align_right,
        return: attribute.class("cell-right"),
        otherwise: attribute.none,
      ),
    ],
    [
      case current_sort_criteria == sort_criteria {
        True -> {
          case current_sort_direction {
            api.Ascending -> html.text("⬆")
            api.Descending -> html.text("⬇")
          }
        }
        False -> html.text("")
      },
      html.text(name),
    ],
  )
}

pub fn map_rows(list: List(a), with fun: fn(a) -> element.Element(b)) {
  map_and_append(
    list,
    fun,
    [],
    html.tr([attribute.class("buffer-row")], [html.td([], [])]),
  )
}

fn map_and_append(l: List(a), fun: fn(a) -> b, acc: List(b), item: b) -> List(b) {
  case l {
    [] -> list.reverse([item, ..acc])
    [x, ..xs] -> map_and_append(xs, fun, [fun(x), ..acc], item)
  }
}
