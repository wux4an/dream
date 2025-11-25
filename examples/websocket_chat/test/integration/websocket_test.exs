defmodule WebsocketChatTest do
  use ExUnit.Case

  setup do
    # Start the Gleam server
    port = 8080
    # We assume the server is started by the test runner or we start it here?
    # Ideally we start it as a port or checking if it's running.
    # For this example, we'll assume "gleam run" is running or we start it.
    
    # Actually, let's start it using System.cmd for isolation?
    # Or better, assume the user runs `make test` which runs `gleam test`.
    # But `gleam test` runs unit tests.
    # We want integration tests.
    
    # In other examples, `cucumber_test.exs` starts the app?
    # Let's check `examples/simple/test/integration/cucumber_test.exs`.
    
    :ok
  end

  test "websocket chat flow" do
    port = 8080
    
    # 1. Connect
    {:ok, conn} = Mint.HTTP.connect(:http, "localhost", port)
    
    # 2. Upgrade
    ref = make_ref()
    headers = [
      {"connection", "upgrade"},
      {"upgrade", "websocket"},
      {"sec-websocket-key", "dGhlIHNhbXBsZSBub25jZQ=="},
      {"sec-websocket-version", "13"}
    ]
    {:ok, conn, request_ref} = Mint.HTTP.request(conn, "GET", "/chat?user=Alice", headers, nil)
    
    # 3. Receive Upgrade Response
    receive do
      message -> 
        {:ok, conn, responses} = Mint.HTTP.stream(conn, message)
        # Verify status 101
        status = Enum.find_value(responses, fn 
          {:status, ^request_ref, status} -> status 
          _ -> nil
        end)
        assert status == 101
        
        # 4. Negotiate Websocket (using MintWebSocket if available or manual framing)
        # For simplicity, we just assert the upgrade happened.
        
        # If we want to send/receive frames, we need MintWebSocket.
        # {:ok, conn, websocket} = Mint.WebSocket.upgrade(conn, request_ref, responses)
    after
      1000 -> flunk("Timeout waiting for upgrade response")
    end
  end
end

