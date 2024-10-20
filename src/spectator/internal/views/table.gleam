//// Generic table rendering view functions

import gleam/bool
import lustre/attribute
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
