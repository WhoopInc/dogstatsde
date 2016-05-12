defmodule BasicTest do
  use ExUnit.Case, async: false

  test "send a metric" do
    assert :ok = Dogstatsd.gauge("test", 1)

    assert :meck.validate(:gen_udp)
    assert :meck.called(:gen_udp, :send, :_)
  end
end
