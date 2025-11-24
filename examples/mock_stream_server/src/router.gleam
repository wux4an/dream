//// router.gleam - Routes for mock stream server
////
//// Defines all streaming endpoints for testing Dream's HTTP client.

import controllers/stream_controller
import dream/context.{type EmptyContext}
import dream/http/request.{Get}
import dream/router.{type EmptyServices, type Router, route, router}

/// Create a router with all mock streaming endpoints
pub fn create_router() -> Router(EmptyContext, EmptyServices) {
  router()
  |> route(
    method: Get,
    path: "/",
    controller: stream_controller.index,
    middleware: [],
  )
  |> route(
    method: Get,
    path: "/stream/fast",
    controller: stream_controller.stream_fast,
    middleware: [],
  )
  |> route(
    method: Get,
    path: "/stream/slow",
    controller: stream_controller.stream_slow,
    middleware: [],
  )
  |> route(
    method: Get,
    path: "/stream/burst",
    controller: stream_controller.stream_burst,
    middleware: [],
  )
  |> route(
    method: Get,
    path: "/stream/error",
    controller: stream_controller.stream_error,
    middleware: [],
  )
  |> route(
    method: Get,
    path: "/stream/huge",
    controller: stream_controller.stream_huge,
    middleware: [],
  )
  |> route(
    method: Get,
    path: "/stream/json",
    controller: stream_controller.stream_json,
    middleware: [],
  )
  |> route(
    method: Get,
    path: "/stream/binary",
    controller: stream_controller.stream_binary,
    middleware: [],
  )
}
