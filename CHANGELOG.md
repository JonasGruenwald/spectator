## [2.1.0] 2026-28-02

This update brings changes that make spectator work better in a real world production environment

- Adjusted timeouts and data collection handling that performs better with nodes distributed across different servers
- Removed option to set cookie via the WebUI
- Implemented check for OTP processes to avoid sending unnecessary system messages to processes that will never handle them
- Bumped dependencies and Gleam version 

Thanks for the contribution and insights from production use [@oderwat](https://github.com/)

## [2.0.1] 2025-10-22

- Embed assets into package instead of relying on priv directory, as it's not available to the escript

## [2.0.0] 2025-10-19

- Interface: Moved the UI to change inspection target to a modal dialog that can be triggered from all pages, and is not rendered through a server component, requiring no active connection
- Restructured the project to allow bundling as a standalone app, escript and docker image
- Breaking change in the public API: `start_on` now takes a host parameter alongside the port, to allow binding to `0.0.0.0` for bundling as an app

## [1.5.0] 2025-10-17

- Never too late to start a changelog!
- Bump dependencies to latest versions, support v1 of `gleam_erlang` and `gleam_otp`, thanks [@vshakitskiy](https://github.com/vshakitskiy)
