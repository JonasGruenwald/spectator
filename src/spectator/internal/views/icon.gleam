import lustre/attribute.{attribute}
import lustre/element/svg

pub fn sort_ascending() {
  svg.svg(
    [
      attribute("viewBox", "0 0 256 256"),
      attribute("fill", "var(--text)"),
      attribute("height", "15"),
      attribute("width", "15"),
      attribute("xmlns", "http://www.w3.org/2000/svg"),
    ],
    [
      svg.path([
        attribute(
          "d",
          "M128,128a12,12,0,0,1-12,12H48a12,12,0,0,1,0-24h68A12,12,0,0,1,128,128ZM48,76H180a12,12,0,0,0,0-24H48a12,12,0,0,0,0,24Zm52,104H48a12,12,0,0,0,0,24h52a12,12,0,0,0,0-24Zm132.49-20.49a12,12,0,0,0-17,0L196,179V112a12,12,0,0,0-24,0v67l-19.51-19.52a12,12,0,0,0-17,17l40,40a12,12,0,0,0,17,0l40-40A12,12,0,0,0,232.49,159.51Z",
        ),
      ]),
    ],
  )
}

pub fn sort_descending() {
  svg.svg(
    [
      attribute("viewBox", "0 0 256 256"),
      attribute("fill", "var(--text)"),
      attribute("height", "15"),
      attribute("width", "15"),
      attribute("xmlns", "http://www.w3.org/2000/svg"),
    ],
    [
      svg.path([
        attribute(
          "d",
          "M36,128a12,12,0,0,1,12-12h68a12,12,0,0,1,0,24H48A12,12,0,0,1,36,128ZM48,76h52a12,12,0,0,0,0-24H48a12,12,0,0,0,0,24ZM180,180H48a12,12,0,0,0,0,24H180a12,12,0,0,0,0-24ZM232.49,79.51l-40-40a12,12,0,0,0-17,0l-40,40a12,12,0,0,0,17,17L172,77v67a12,12,0,0,0,24,0V77l19.51,19.52a12,12,0,0,0,17-17Z",
        ),
      ]),
    ],
  )
}
