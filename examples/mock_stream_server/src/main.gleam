//// main.gleam - Mock Stream Server
////
//// Standalone entry point for the mock stream server.
//// Provides various streaming endpoints for testing and demonstrating
//// Dream's HTTP client streaming capabilities.

import dream/servers/mist/server.{bind, listen, router} as dream
import router.{create_router}

pub fn main() {
  dream.new()
  |> router(create_router())
  |> bind("localhost")
  |> listen(3004)
}
