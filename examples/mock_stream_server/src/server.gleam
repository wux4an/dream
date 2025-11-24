//// server.gleam - Programmatic server control
////
//// Provides functions to start and stop the mock stream server programmatically.
//// Used by integration tests for the HTTP client module.

import dream/servers/mist/server.{listen_with_handle, router} as dream_server
import gleam/otp/actor
import router.{create_router}

/// Start the mock stream server on a specific port
///
/// Returns a ServerHandle that can be used to stop the server later.
/// Port must be explicitly provided.
///
/// ## Example
///
/// ```gleam
/// import mock_stream_server/server
///
/// let assert Ok(handle) = server.start(3004)
/// // ... make requests ...
/// server.stop(handle)
/// ```
pub fn start(port: Int) -> Result(dream_server.ServerHandle, actor.StartError) {
  dream_server.new()
  |> router(create_router())
  |> listen_with_handle(port)
}

/// Stop the mock stream server
///
/// Gracefully stops the server using the provided handle.
///
/// ## Example
///
/// ```gleam
/// import mock_stream_server/server
///
/// let assert Ok(handle) = server.start(3004)
/// server.stop(handle)
/// ```
pub fn stop(handle: dream_server.ServerHandle) -> Nil {
  dream_server.stop(handle)
}
