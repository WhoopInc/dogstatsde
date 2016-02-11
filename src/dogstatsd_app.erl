-module(dogstatsd_app).
-behavior(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    Config = [
              {agent_address, "AGENT_ADDRESS", [{default, "localhost"}]}
             ,{agent_port, "AGENT_PORT", [{default, 8126}, {transform, integer}]}
             ],
    ok = stillir:set_config(dogstatsd, Config),
    dogstatsd_sup:start_link().

stop(_State) ->
    ok.
