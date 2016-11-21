defmodule ElixirExample do
  require Logger

  def main(_) do
    Logger.info "Example dogstatsd calls"
    :dogstatsd.event("Test", "Testing dogstatsd library", :info, :low)
    :dogstatsd.gauge("connection.active", 5)
    :dogstatsd.increment("requests.image", 1, %{:file_type => "png"})
    :dogstatsd.timer("connection.latency", 35, %{:path => "/hello/world.png"})
    :dogstatsd.set("user.recent_ids", 2054)
  end
end
