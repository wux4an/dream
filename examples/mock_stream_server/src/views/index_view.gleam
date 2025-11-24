//// index_view.gleam - View for mock stream server documentation
////
//// Pure formatting functions - no Response objects.

/// Format index page with endpoint documentation
pub fn format_index() -> String {
  "Mock Stream Server - Testing & Demo Endpoints\n\n"
  <> "This server provides controllable streaming endpoints for testing Dream's HTTP client.\n\n"
  <> "Available Endpoints:\n\n"
  <> "  GET /stream/fast\n"
  <> "    10 chunks at 100ms intervals\n"
  <> "    Use for: Testing fast streaming\n\n"
  <> "  GET /stream/slow\n"
  <> "    5 chunks at 2s intervals\n"
  <> "    Use for: Testing timeout handling\n\n"
  <> "  GET /stream/burst\n"
  <> "    7 chunks with variable delays (100-500ms)\n"
  <> "    Use for: Testing variable timing patterns\n\n"
  <> "  GET /stream/error\n"
  <> "    3 chunks then 500 status\n"
  <> "    Use for: Testing error handling mid-stream\n\n"
  <> "  GET /stream/huge\n"
  <> "    100 chunks with 10ms delays\n"
  <> "    Use for: Memory and performance testing\n\n"
  <> "  GET /stream/json\n"
  <> "    5 JSON objects at 200ms intervals\n"
  <> "    Use for: Testing structured data streaming\n\n"
  <> "  GET /stream/binary\n"
  <> "    256 binary chunks with byte patterns\n"
  <> "    Use for: Testing non-text streaming\n\n"
  <> "Server: localhost:3004\n"
  <> "Content-Type: All endpoints return appropriate MIME types\n"
}
