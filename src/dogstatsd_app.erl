-module(dogstatsd_app).
-behavior(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    Config = [
              gen_config_for(agent_address, "localhost")
             ,gen_config_for(agent_port, 8125, integer)
             ,gen_config_for(send_metrics, true, fun transform_boolean/1)
             ,gen_config_for(global_prefix, "")
             ,gen_config_for(global_tags, #{}, fun transform_map/1)
             ,gen_config_for(vm_stats, true, fun transform_boolean/1)
             ,gen_config_for(vm_stats_delay, 60000, integer)
             ,gen_config_for(vm_stats_scheduler, true, fun transform_boolean/1)
             ,gen_config_for(vm_stats_base_key, "erlang.vm")
             ],
    ok = stillir:set_config(dogstatsd, Config),
    dogstatsd_sup:start_link().

gen_config_for(Key, Default) -> gen_config_for(Key, Default, undefined).

gen_config_for(Key, Default0, Transform) ->
    Default1 = case application:get_env(dogstatsd, Key) of
        undefined -> Default0;
        {ok, Value} -> Value
    end,
    Options0 = [{default, Default1}],

    Options1 = case Transform =:= undefined of
        true -> Options0;
        false -> [{transform, Transform} | Options0]
    end,

    EnvVarName = string:to_upper(erlang:atom_to_list(Key)),

    {Key, EnvVarName, Options1}.

stop(_State) ->
    ok.

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

-endif.
