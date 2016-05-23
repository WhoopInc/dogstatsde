ExUnit.start

Application.ensure_all_started(:meck)

:meck.new(:gen_udp, [:unstick])
:meck.expect(:gen_udp, :send, fn (_Socket, _Address, _Port, _Packet) -> :ok end)
:meck.expect(:gen_udp, :open, fn (_Port) -> {:ok, :fake_socket} end)

Dogstatsd.start
