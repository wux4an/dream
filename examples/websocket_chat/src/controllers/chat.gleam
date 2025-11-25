import dream/context.{type EmptyContext}
import dream/http/request.{type Request}
import dream/http/response.{type Response}
import dream/servers/mist/websocket
import dream/services/broadcaster
import gleam/erlang/process
import gleam/json
import gleam/option.{Some}
import mist
import services.{type Services}
import types/chat_message.{type ChatMessage, TextMessage, UserJoined, UserLeft}

/// Handle WebSocket upgrade request for chat
///
/// Upgrades the HTTP request to a WebSocket connection and sets up
/// pub/sub messaging for the chat room.
///
/// ## Example
///
/// ```gleam
/// router()
/// |> route(Get, "/chat", controllers.chat.handle_chat_upgrade, [])
/// ```
pub fn handle_chat_upgrade(
  request: Request,
  _context: EmptyContext,
  services: Services,
) -> Response {
  let user = request.get_query_param(request.query, "user")
  let user = option.unwrap(user, "Anonymous")

  websocket.upgrade(
    request,
    on_init: create_websocket_init_handler(user, services),
    handler: create_websocket_message_handler(services),
    on_close: create_websocket_close_handler(services),
  )
}

/// Create the WebSocket initialization handler
///
/// Note: This function returns a closure that captures `user` and `services`.
/// This is necessary because Mist's websocket API requires a function with a
/// specific signature that doesn't allow passing these dependencies explicitly.
/// This is an exception to the "no closures" rule per the Third-Party Module
/// Requirements exception in Dream's coding standards.
fn create_websocket_init_handler(
  user: String,
  services: Services,
) -> fn(websocket.Connection) ->
  #(String, option.Option(process.Selector(ChatMessage))) {
  fn(_connection) {
    // 1. Subscribe to broadcaster
    let channel = broadcaster.subscribe(services.pubsub)

    // 2. Create selector for chat messages
    let selector = broadcaster.channel_to_selector(channel)

    // 3. Notify join
    broadcaster.publish(services.pubsub, UserJoined(user))

    // Initial state is the username
    #(user, Some(selector))
  }
}

/// Create the WebSocket message handler
///
/// Note: This function returns a closure that captures `services`.
/// This is necessary because Mist's websocket API requires a function with a
/// specific signature that doesn't allow passing this dependency explicitly.
/// This is an exception to the "no closures" rule per the Third-Party Module
/// Requirements exception in Dream's coding standards.
fn create_websocket_message_handler(services: Services) {
  fn(
    state: String,
    message: websocket.Message(ChatMessage),
    connection: websocket.Connection,
  ) {
    case message {
      mist.Text(text) -> {
        // Broadcast user message
        broadcaster.publish(services.pubsub, TextMessage(state, text))
        websocket.continue(state)
      }
      mist.Custom(chat_message) -> {
        // Received from PubSub, send to client
        let json_message = format_chat_message_to_json(chat_message)
        let _ = websocket.send_text(connection, json.to_string(json_message))
        websocket.continue(state)
      }
      mist.Binary(_) | mist.Closed | mist.Shutdown -> websocket.continue(state)
    }
  }
}

/// Format a chat message to JSON
fn format_chat_message_to_json(chat_message: ChatMessage) -> json.Json {
  case chat_message {
    TextMessage(user, text) ->
      json.object([
        #("type", json.string("message")),
        #("user", json.string(user)),
        #("text", json.string(text)),
      ])
    UserJoined(user) ->
      json.object([
        #("type", json.string("joined")),
        #("user", json.string(user)),
      ])
    UserLeft(user) ->
      json.object([
        #("type", json.string("left")),
        #("user", json.string(user)),
      ])
  }
}

/// Create the WebSocket close handler
///
/// Note: This function returns a closure that captures `services`.
/// This is necessary because Mist's websocket API requires a function with a
/// specific signature that doesn't allow passing this dependency explicitly.
/// This is an exception to the "no closures" rule per the Third-Party Module
/// Requirements exception in Dream's coding standards.
fn create_websocket_close_handler(services: Services) {
  fn(state: String) { broadcaster.publish(services.pubsub, UserLeft(state)) }
}
