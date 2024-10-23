import gleam/erlang/process
import gleam/option.{type Option, Some}
import lustre
import lustre/effect
import lustre/element.{type Element}
import lustre/element/html
import spectator/internal/common

// MAIN ------------------------------------------------------------------------

pub fn app() {
  lustre.application(init, update, view)
}

// MODEL -----------------------------------------------------------------------

pub type Model {
  Model(subject: Option(process.Subject(Msg)))
}

fn init(_initial_selection: Option(String)) -> #(Model, effect.Effect(Msg)) {
  #(
    Model(subject: option.None),
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

fn do_refresh(model: Model) -> Model {
  model
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
    CreatedSubject(subject) -> #(Model(subject: Some(subject)), effect.none())
  }
}

// VIEW ------------------------------------------------------------------------

fn view(_model: Model) -> Element(Msg) {
  html.text("helo")
}
