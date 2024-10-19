import gleam/dynamic
import gleam/erlang/process
import gleam/option.{type Option, None, Some}
import lustre
import lustre/effect
import lustre/element.{type Element}
import lustre/server_component
import spectator/internal/api
import spectator/internal/views/process_table

const interval = 500

// MAIN ------------------------------------------------------------------------

pub fn app() {
  lustre.application(init, update, view)
}

// MODEL -----------------------------------------------------------------------

pub type Model {
  Model(
    subject: Option(process.Subject(Msg)),
    process_list: List(api.ProcessItem),
    sort_criteria: api.InfoSortCriteria,
    sort_direction: api.SortDirection,
    active_process: Option(api.ProcessItem),
    details: Option(api.ProcessDetails),
    status: Option(api.Status),
    state: Option(dynamic.Dynamic),
  )
}

pub type ProcessesLive =
  process.Subject(lustre.Action(Msg, lustre.ServerComponent))

fn emit_after(
  delay: Int,
  msg: Msg,
  subject: Option(process.Subject(Msg)),
) -> effect.Effect(Msg) {
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
      // dispatch a message so you can store this `subject` and
      // re-use it.
      dispatch(CreatedSubject(subject))
      selector
    }
  }
}

fn request_otp_details(
  pid: process.Pid,
  subject: Option(process.Subject(Msg)),
) -> effect.Effect(Msg) {
  case subject {
    Some(sub) -> {
      use _ <- effect.from
      api.request_otp_data(pid, fn(details) {
        process.send(sub, ReceivedOtpDetails(details))
      })
      Nil
    }
    None -> effect.none()
  }
}

fn init(_) -> #(Model, effect.Effect(Msg)) {
  let info = api.get_info_list()
  let default_sort_criteria = api.Reductions
  let default_sort_direction = api.Descending
  let sorted = api.sort_info_list(info, default_sort_criteria, api.Descending)
  #(
    Model(
      subject: option.None,
      process_list: sorted,
      sort_criteria: default_sort_criteria,
      sort_direction: default_sort_direction,
      active_process: option.None,
      details: option.None,
      status: option.None,
      state: option.None,
    ),
    emit_after(interval, Refresh, option.None),
  )
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  Refresh
  ReceivedOtpDetails(details: api.OtpDetails)
  CreatedSubject(process.Subject(Msg))
  ProcessClicked(api.ProcessItem)
  HeadingClicked(api.InfoSortCriteria)
  OtpStateClicked(api.ProcessItem, api.SysState)
}

fn do_refresh(model: Model) -> Model {
  let info = api.get_info_list()
  let sorted =
    api.sort_info_list(info, model.sort_criteria, model.sort_direction)

  let active_process = case model.active_process {
    None -> None
    Some(active_process) -> {
      case api.get_info(active_process.pid) {
        Ok(info) -> Some(api.ProcessItem(active_process.pid, info))
        Error(_) -> None
      }
    }
  }

  let details = case active_process {
    None -> None
    Some(ap) -> {
      case api.get_details(ap.pid) {
        Ok(details) -> Some(details)
        Error(_) -> None
      }
    }
  }
  Model(..model, active_process:, process_list: sorted, details:)
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    Refresh -> {
      let new_model = do_refresh(model)
      case new_model.active_process {
        Some(p) -> #(new_model, request_otp_details(p.pid, model.subject))
        None -> #(new_model, effect.none())
      }
    }
    CreatedSubject(subject) -> #(
      Model(..model, subject: Some(subject)),
      effect.none(),
    )
    ProcessClicked(p) -> {
      let new_model =
        Model(..model, active_process: Some(p), state: None, status: None)
        |> do_refresh
      #(new_model, request_otp_details(p.pid, model.subject))
    }
    HeadingClicked(criteria) -> {
      case criteria {
        c if c == model.sort_criteria -> {
          let new_model =
            Model(
              ..model,
              sort_direction: api.invert_sort_direction(model.sort_direction),
            )
          #(do_refresh(new_model), effect.none())
        }
        c -> {
          let new_model = Model(..model, sort_criteria: c)
          #(do_refresh(new_model), effect.none())
        }
      }
    }
    ReceivedOtpDetails(details) -> {
      case model.active_process {
        Some(p) if p.pid == details.pid -> {
          #(
            Model(
              ..model,
              state: Some(details.state),
              status: Some(details.status),
            ),
            effect.none(),
          )
        }
        _ -> #(model, effect.none())
      }
    }
    OtpStateClicked(p, target_sys_state) -> {
      case target_sys_state {
        api.Suspended -> {
          api.resume(p.pid)
          #(do_refresh(model), request_otp_details(p.pid, model.subject))
        }
        api.Running -> {
          api.suspend(p.pid)
          #(do_refresh(model), request_otp_details(p.pid, model.subject))
        }
      }
    }
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  process_table.render(
    model.process_list,
    ProcessClicked,
    model.active_process,
    model.details,
    model.status,
    model.state,
    model.sort_criteria,
    model.sort_direction,
    HeadingClicked,
    OtpStateClicked,
  )
}
