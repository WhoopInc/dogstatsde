-module(dogstatsd_app).
-behavior(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    configure(),
    dogstatsd_sup:start_link().

stop(_State) ->
    ok.

configure() ->
    Config = [
              {agent_address, "AGENT_ADDRESS", [{default, "localhost"}]}
             ,{agent_port, "AGENT_PORT", [{default, 8125}, {transform, integer}]}
             ,{send_metrics, "SEND_METRICS", [{default, true}, {transform, fun transform_boolean/1}]}
             ,{global_prefix, "GLOBAL_PREFIX", [{default, ""}]}
             ,{global_tags, "GLOBAL_TAGS", [{default, #{}}, {transform, fun transform_map/1}]}
             ,{vm_stats, "VM_STATS", [{default, true}, {transform, fun transform_boolean/1}]}
             ,{vm_stats_delay, "VM_STATS_DELAY", [{default, 60000}, {transform, integer}]}
             ,{vm_stats_scheduler, "VM_STATS_SCHEDULER", [{default, true}, {transform, fun transform_boolean/1}]}
             ,{vm_stats_base_key, "VM_STATS_BASE_KEY", [{default, "erlang.vm"}]}
             ],
    Config1 = read_app_config(Config),
    ok = stillir:set_config(dogstatsd, Config1).

read_app_config(Config) ->
    lists:map(fun ({AppVar, EnvVar, Opts0}) ->
                      Opts1 = case application:get_env(dogstatsd, AppVar) of
                                  {ok, Value} ->
                                      lists:keystore(default, 1, Opts0, {default, Value});
                                  undefined ->
                                      Opts0
                              end,
                      {AppVar, EnvVar, Opts1}
              end,
              Config).

transform_map(String) ->
    CompressedString = re:replace(String, <<" ">>, <<>>, [global]),
    TrimmedString = case re:run(CompressedString, <<"#{(.*)}">>, [{capture, all_but_first, list}]) of
                        nomatch ->
                            CompressedString;
                        {match, [Inner]} ->
                            Inner
                    end,
    lists:foldl(fun (<<>>, Acc) ->
                        Acc;
                    (El, Acc) ->
                        io:format("El=~p", [El]),
                        case re:split(El, <<"=>?">>, [{return, binary}]) of
                            [Key, Value] ->
                                maps:put(Key, Value, Acc);
                            Other ->
                                erlang:error({cannot_parse_kv_pair, Other})
                        end
                end,
                #{},
                re:split(TrimmedString, <<$,>>, [{return, binary}])).

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

%%% Tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

transform_boolean_test_() -> [
                              ?_assert(transform_boolean("true"))
                             ,?_assert(transform_boolean("TRUE"))
                             ,?_assert(transform_boolean("True"))
                             ,?_assert(transform_boolean("Yes"))
                             ,?_assert(transform_boolean("1"))
                             ,?_assertNot(transform_boolean("0"))
                             ,?_assertNot(transform_boolean("NO"))
                             ,?_assertNot(transform_boolean("false"))
                             ,?_assertNot(transform_boolean("FALSE"))
                             ,?_assertError({not_a_boolean, "nope"}, transform_boolean("nope"))
                             ,?_assertError({not_a_boolean, ""}, transform_boolean(""))
                             ,?_assertError(function_clause, transform_boolean(0))
                             ].

transform_map_test_() -> [
                          ?_assertEqual(#{<<"hello">> => <<"world">>}, transform_map("#{hello=>world}"))
                         ,?_assertEqual(#{<<"hello">> => <<"world">>}, transform_map("hello=world"))
                         ,?_assertEqual(#{<<"hello">> => <<"world">>}, transform_map("hello = world"))
                         ,?_assertEqual(#{<<"hello">> => <<"world">>}, transform_map("hello=> world"))
                         ,?_assertEqual(#{<<"a">> => <<"1">>, <<"b">> => <<"2">>, <<"c">> => <<"3">>},
                                        transform_map("#{a=>1,b=>2,c=>3}"))
                         ,?_assertEqual(#{<<"hello">> => <<"world">>}, transform_map("#{ hello => world }"))
                         ,?_assertEqual(#{}, transform_map(""))
                         ,?_assertError({cannot_parse_kv_pair, _}, transform_map("hello"))
                         ,?_assertError({cannot_parse_kv_pair, _}, transform_map("#{hello}"))
                         ,?_assertError({cannot_parse_kv_pair, _}, transform_map("#{hello,world}"))
                         ].

supervisor_test_() ->
    {setup,
     fun () ->
             application:ensure_all_started(worker_pool),
             configure()
     end,
     fun (_) ->
             application:stop(worker_pool)
     end,
     [
      ?_assertMatch({ok,_Pid}, dogstatsd_sup:start_link())
     ]}.

application_test_() ->
    {setup,
     fun () ->
             meck:new(gen_udp, [unstick]),
             meck:expect(gen_udp, send, fun (_Socket, _Address, _Port, _Packet) -> ok end),
             meck:expect(gen_udp, open, fun (_Port) -> {ok, fake_socket} end),
             application:ensure_all_started(dogstatsd)
     end,
     fun (_) ->
             meck:unload(gen_udp),
             application:stop(worker_pool)
     end,
     [
      ?_assertMatch(ok, dogstatsd:gauge("test", 1))
     ,?_assert(meck:validate(gen_udp))
     ,?_assert(meck:called(gen_udp, send, '_'))
     ]}.

-endif.
