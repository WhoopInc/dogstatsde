-module(dogstatsd_app).
-behavior(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    Config = [
              {agent_address, "AGENT_ADDRESS", [{default, "localhost"}]}
             ,{agent_port, "AGENT_PORT", [{default, 8125}, {transform, integer}]}
             ,{send_metrics, "SEND_METRICS", [{default, true}, {transform, fun transform_boolean/1}]}
             ,{vm_stats, "VM_STATS", [{default, true}, {transform, fun transform_boolean/1}]}
             ,{vm_stats_delay, "VM_STATS_DELAY", [{default, 60000}, {transform, integer}]}
             ,{vm_stats_scheduler, "VM_STATS_SCHEDULER", [{default, true}, {transform, fun transform_boolean/1}]}
             ,{vm_stats_base_key, "VM_STATS_BASE_KEY", [{default, "erlang.vm"}]}
             ],
    ok = stillir:set_config(dogstatsd, Config),
    dogstatsd_sup:start_link().

stop(_State) ->
    ok.

transform_boolean(String) ->
    case string:to_lower(string:strip(String)) of
        "true" ->
            true;
        "yes" ->
            true;
        "1" ->
            true;
        "false" ->
            false;
        "no" ->
            false;
        "0" ->
            false;
        _ ->
            erlang:error({not_a_boolean, String})
    end.
