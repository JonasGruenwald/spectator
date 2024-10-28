#!/bin/bash
export ERL_FLAGS="-sname spectator"
watchexec --restart --verbose --wrap-process=session --stop-signal SIGTERM --exts gleam --debounce 500ms --watch src/ -- "gleam run -m playground"