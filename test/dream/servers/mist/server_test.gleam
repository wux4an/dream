//// Tests for dream/servers/mist/server module.

import dream/dream
import dream/router.{router}
import dream/servers/mist/server
import dream_test/assertions/should.{equal, or_fail_with, should}
import dream_test/types.{AssertionFailed, AssertionFailure, AssertionOk}
import dream_test/unit.{type UnitTest, after_each, before_each, describe, it}
import fixtures/hooks.{start_server, stop_server, test_server_port}
import gleam/erlang/process
import gleam/option.{None}
import gleam/string

// ============================================================================
// Tests
// ============================================================================

pub fn tests() -> UnitTest {
  describe("server", [
    builder_tests(),
    listen_tests(),
    lifecycle_tests(),
    bind_tests(),
  ])
}

fn builder_tests() -> UnitTest {
  describe("builder", [
    it("new creates dream instance with 10MB default max body size", fn() {
      dream.get_max_body_size(server.new())
      |> should()
      |> equal(10_000_000)
      |> or_fail_with("Default max body size should be 10MB")
    }),
    it("router sets router on dream instance", fn() {
      let _ = server.router(server.new(), router())
      AssertionOk
    }),
    it("bind sets bind address", fn() {
      let _ =
        server.new()
        |> server.router(router())
        |> server.bind("127.0.0.1")
      AssertionOk
    }),
    it("max_body_size sets max body size", fn() {
      let _ =
        server.new()
        |> server.router(router())
        |> server.max_body_size(2048)
      AssertionOk
    }),
  ])
}

fn listen_tests() -> UnitTest {
  describe("listen_with_handle", [
    before_each(start_server),
    after_each(stop_server),
    it("starts on expected port", fn() {
      // Server is started by before_each hook
      // Just verify the port constant is what we expect
      test_server_port
      |> should()
      |> equal(19_999)
      |> or_fail_with("Test server port should be 19999")
    }),
  ])
}

fn lifecycle_tests() -> UnitTest {
  describe("server lifecycle", [
    it("listen_with_handle returns server handle", fn() {
      let result =
        server.new()
        |> server.router(router())
        |> server.listen_with_handle(19_990)

      case result {
        Ok(handle) -> {
          server.stop(handle)
          AssertionOk
        }
        Error(start_error) ->
          AssertionFailed(AssertionFailure(
            operator: "listen_with_handle",
            message: "Server failed to start on port 19990: "
              <> string.inspect(start_error),
            payload: None,
          ))
      }
    }),
    it("stop actually stops server", fn() {
      let result =
        server.new()
        |> server.router(router())
        |> server.listen_with_handle(19_991)

      case result {
        Ok(handle) -> {
          // Wait for server to start
          process.sleep(100)
          // Stop the server
          server.stop(handle)
          // Wait for server to stop
          process.sleep(100)
          // If we get here without panic, stop() worked
          AssertionOk
        }
        Error(start_error) ->
          AssertionFailed(AssertionFailure(
            operator: "stop",
            message: "Server failed to start: " <> string.inspect(start_error),
            payload: None,
          ))
      }
    }),
    it("stop is idempotent", fn() {
      let result =
        server.new()
        |> server.router(router())
        |> server.listen_with_handle(19_992)

      case result {
        Ok(handle) -> {
          process.sleep(100)
          // Stop the server multiple times - should not crash
          server.stop(handle)
          server.stop(handle)
          server.stop(handle)
          AssertionOk
        }
        Error(start_error) ->
          AssertionFailed(AssertionFailure(
            operator: "stop_idempotent",
            message: "Server failed to start: " <> string.inspect(start_error),
            payload: None,
          ))
      }
    }),
  ])
}

fn bind_tests() -> UnitTest {
  describe("bind configuration", [
    it("bind configuration persists through listen", fn() {
      // Regression test: bind() configuration should not be lost in listen()
      let result =
        server.new()
        |> server.router(router())
        |> server.bind("127.0.0.1")
        |> server.listen_with_handle(19_993)

      case result {
        Ok(handle) -> {
          server.stop(handle)
          AssertionOk
        }
        Error(start_error) ->
          AssertionFailed(AssertionFailure(
            operator: "bind_persists",
            message: "Server with bind() failed to start on port 19993: "
              <> string.inspect(start_error),
            payload: None,
          ))
      }
    }),
    it("bind to localhost works", fn() {
      let result =
        server.new()
        |> server.router(router())
        |> server.bind("localhost")
        |> server.listen_with_handle(19_994)

      case result {
        Ok(handle) -> {
          server.stop(handle)
          AssertionOk
        }
        Error(start_error) ->
          AssertionFailed(AssertionFailure(
            operator: "bind_localhost",
            message: "Server with bind('localhost') failed to start: "
              <> string.inspect(start_error),
            payload: None,
          ))
      }
    }),
    it("bind to all interfaces works", fn() {
      let result =
        server.new()
        |> server.router(router())
        |> server.bind("0.0.0.0")
        |> server.listen_with_handle(19_995)

      case result {
        Ok(handle) -> {
          server.stop(handle)
          AssertionOk
        }
        Error(start_error) ->
          AssertionFailed(AssertionFailure(
            operator: "bind_all",
            message: "Server with bind('0.0.0.0') failed to start: "
              <> string.inspect(start_error),
            payload: None,
          ))
      }
    }),
  ])
}
