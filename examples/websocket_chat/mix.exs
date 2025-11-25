defmodule WebsocketChatExample.MixProject do
  use Mix.Project

  def project do
    [
      app: :websocket_chat_example,
      version: "0.1.0",
      elixir: "~> 1.18",
      start_permanent: Mix.env() == :prod,
      test_paths: ["test/integration"],
      test_pattern: "*_test.exs",
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:mint, "~> 1.5", only: [:test]},
      {:mint_web_socket, "~> 1.0", only: [:test]},
      {:castore, "~> 1.0", only: [:test]}
    ]
  end
end

