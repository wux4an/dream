import dream/http/request.{type Request}
import dream/http/response.{type Response, empty_response}
import dream/servers/mist/internal
import gleam/erlang/atom
import gleam/erlang/process
import gleam/http/request as http_request
import gleam/option.{type Option, Some}
import glisten
import mist.{type WebsocketConnection, type WebsocketMessage}

// Re-export Mist types for isolation
pub type Connection =
  WebsocketConnection

pub type Message(msg) =
  WebsocketMessage(msg)

pub type Next(state, msg) =
  mist.Next(state, msg)

/// Upgrade the current HTTP request to a WebSocket connection.
///
/// This function must be called from within a Dream controller. It bypasses the standard
/// response flow to perform the upgrade using the underlying Mist connection.
///
/// ## Example
///
/// ```gleam
/// websocket.upgrade(
///   request,
///   on_init: fn(conn) { #(initial_state, None) },
///   handler: handle_message,
///   on_close: handle_close,
/// )
/// ```
///
/// ## Arguments
///
/// * `request` - The current Dream request (used for type checking, though the upgrade uses the underlying connection)
/// * `on_init` - Function called when the WebSocket connects. Should return initial state and optional selector.
/// * `handler` - Function called for each WebSocket message.
/// * `on_close` - Function called when the connection closes.
///
/// ## Returns
///
/// Returns a 200 OK response. The Dream handler will intercept this and return the
/// actual WebSocket upgrade response to the client.
pub fn upgrade(
  request _request: Request,
  on_init on_init: fn(WebsocketConnection) ->
    #(state, Option(process.Selector(msg))),
  handler handler: fn(state, Message(msg), WebsocketConnection) ->
    Next(state, msg),
  on_close on_close: fn(state) -> Nil,
) -> Response {
  // 1. Retrieve stashed Mist request
  let request_key = atom.create(internal.request_key)
  let raw_request = internal.get(request_key)

  // 2. Cast to Mist Request (unsafe but guaranteed by our handler)
  // Note: Mist's websocket function expects Request(mist.Connection), not Request(WebsocketConnection)
  let mist_request: http_request.Request(mist.Connection) =
    internal.unsafe_coerce(raw_request)

  // 3. Perform upgrade logic
  let mist_response =
    mist.websocket(
      request: mist_request,
      on_init: on_init,
      handler: handler,
      on_close: on_close,
    )

  // 4. Stash the result
  let response_key = atom.create(internal.response_key)
  internal.put(response_key, internal.unsafe_coerce(Some(mist_response)))

  // 5. Return dummy response to satisfy Dream controller signature
  empty_response(200)
}

/// Continue the WebSocket loop with the current state
pub fn continue(state: state) -> Next(state, msg) {
  mist.continue(state)
}

/// Continue the WebSocket loop with a selector for receiving custom messages
pub fn continue_with_selector(
  state: state,
  selector: process.Selector(msg),
) -> Next(state, msg) {
  mist.continue(state)
  |> mist.with_selector(selector)
}

/// Stop the WebSocket loop normally
pub fn stop() -> Next(state, msg) {
  mist.stop()
}

/// Send a text frame to the client
///
/// ## Example
///
/// ```gleam
/// case websocket.send_text(connection, "Hello, World!") {
///   Ok(Nil) -> // Message sent successfully
///   Error(reason) -> // Handle send error
/// }
/// ```
pub fn send_text(
  connection: WebsocketConnection,
  message: String,
) -> Result(Nil, glisten.SocketReason) {
  mist.send_text_frame(connection, message)
}

/// Send a binary frame to the client
///
/// ## Example
///
/// ```gleam
/// case websocket.send_binary(connection, image_data) {
///   Ok(Nil) -> // Message sent successfully
///   Error(reason) -> // Handle send error
/// }
/// ```
pub fn send_binary(
  connection: WebsocketConnection,
  message: BitArray,
) -> Result(Nil, glisten.SocketReason) {
  mist.send_binary_frame(connection, message)
}
