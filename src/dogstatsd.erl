-module(dogstatsd).

-type metric_name() :: iodata().
-type metric_value() :: number().
-type metric_type() :: counter | gauge | histogram | timer | set.
-type metric_sample_rate() :: number().
-type metric_tags() :: map().

-type metric_data() :: {metric_name(), metric_value()}
    | {metric_name(), metric_value(), metric_sample_rate()|metric_tags()}
    | {metric_name(), metric_value(), metric_sample_rate(), metric_tags()}.

-type event_title() :: iodata().
-type event_text() :: iodata().
-type event_type() :: info | error | warning | success.
-type event_priority() :: normal | low.
-type event_tags() :: map().

-export_type([
              metric_name/0
             ,metric_value/0
             ,metric_type/0
             ,metric_sample_rate/0
             ,metric_tags/0
             ,metric_data/0
             ]).

-export([
        gauge/1, gauge/2, gauge/3, gauge/4
        ,counter/1 ,counter/2, counter/3, counter/4
        ,increment/1, increment/2, increment/3, increment/4
        ,histogram/1, histogram/2, histogram/3, histogram/4
        ,timer/1, timer/2, timer/3, timer/4
        ,timing/1, timing/2, timing/3, timing/4
        ,set/1, set/2, set/3, set/4
        ,event/1, event/2, event/3, event/4, event/5
        ]).

-spec send_metric(metric_type(), [metric_data()]) -> ok.
send_metric(_Type, []) ->
    ok;
send_metric(Type, MetricDataList) ->
    NormalizedMetricDataList = [normalize_metric_data(MetricData) || MetricData <- MetricDataList],
    send({metric, {Type, NormalizedMetricDataList}}).

-spec send_event(event_title(), event_text(), event_type(), event_priority(), event_tags()) -> ok.
send_event(Title, Text, Type, Priority, Tags) ->
    send({event, {Title, Text, Type, Priority, Tags}}).

-spec send({atom(), tuple()}) -> ok.
send(Data) ->
    wpool:cast(dogstatsd_worker, Data).

-define(SPEC_TYPE_1(Type), -spec Type(metric_data() | [metric_data()]) -> ok).
-define(MK_TYPE_1(Type),
        Type(MetricDataList) when is_list(MetricDataList) ->
               send_metric(Type, MetricDataList);
        Type(MetricData) when is_tuple(MetricData) ->
               send_metric(Type, [MetricData])
).
-define(SPEC_TYPE_2(Type), -spec Type(metric_name(), metric_value()) -> ok).
-define(MK_TYPE_2(Type),
        Type(Name, Value) when is_number(Value) ->
               send_metric(Type, [{Name, Value}])
).
-define(SPEC_TYPE_3(Type), -spec Type(metric_name(), metric_value(), metric_sample_rate()|metric_tags()) -> ok).
-define(MK_TYPE_3(Type),
        Type(Name, Value, SampleRateOrTags) when is_number(Value) andalso (is_number(SampleRateOrTags) orelse is_map(SampleRateOrTags)) ->
               send_metric(Type, [{Name, Value, SampleRateOrTags}])
).
-define(SPEC_TYPE_4(Type), -spec Type(metric_name(), metric_value(), metric_sample_rate(), metric_tags()) -> ok).
-define(MK_TYPE_4(Type),
        Type(Name, Value, SampleRate, Tags) when is_number(SampleRate), is_map(Tags) ->
               send_metric(Type, [{Name, Value, SampleRate, Tags}])
).

-define(ALIAS_TYPE_1(Alias, Real), Alias(A) -> Real(A)).
-define(ALIAS_TYPE_2(Alias, Real), Alias(A, B) -> Real(A, B)).
-define(ALIAS_TYPE_3(Alias, Real), Alias(A, B, C) -> Real(A, B, C)).
-define(ALIAS_TYPE_4(Alias, Real), Alias(A, B, C, D) -> Real(A, B, C, D)).

?SPEC_TYPE_1(gauge).
?SPEC_TYPE_2(gauge).
?SPEC_TYPE_3(gauge).
?SPEC_TYPE_4(gauge).
?MK_TYPE_1(gauge).
?MK_TYPE_2(gauge).
?MK_TYPE_3(gauge).
?MK_TYPE_4(gauge).

?SPEC_TYPE_1(counter).
?SPEC_TYPE_2(counter).
?SPEC_TYPE_3(counter).
?SPEC_TYPE_4(counter).
?MK_TYPE_1(counter).
?MK_TYPE_2(counter).
?MK_TYPE_3(counter).
?MK_TYPE_4(counter).
?ALIAS_TYPE_1(increment, counter).
?ALIAS_TYPE_2(increment, counter).
?ALIAS_TYPE_3(increment, counter).
?ALIAS_TYPE_4(increment, counter).

?SPEC_TYPE_1(histogram).
?SPEC_TYPE_2(histogram).
?SPEC_TYPE_3(histogram).
?SPEC_TYPE_4(histogram).
?MK_TYPE_1(histogram).
?MK_TYPE_2(histogram).
?MK_TYPE_3(histogram).
?MK_TYPE_4(histogram).

?SPEC_TYPE_1(timer).
?SPEC_TYPE_2(timer).
?SPEC_TYPE_3(timer).
?SPEC_TYPE_4(timer).
?MK_TYPE_1(timer).
?MK_TYPE_2(timer).
?MK_TYPE_3(timer).
?MK_TYPE_4(timer).
?ALIAS_TYPE_1(timing, timer).
?ALIAS_TYPE_2(timing, timer).
?ALIAS_TYPE_3(timing, timer).
?ALIAS_TYPE_4(timing, timer).

?SPEC_TYPE_1(set).
?SPEC_TYPE_2(set).
?SPEC_TYPE_3(set).
?SPEC_TYPE_4(set).
?MK_TYPE_1(set).
?MK_TYPE_2(set).
?MK_TYPE_3(set).
?MK_TYPE_4(set).

-spec event(event_title()) -> ok.
event(Title) -> event(Title, "").
-spec event(event_title(), event_text()) -> ok.
event(Title, Text) -> event(Title, Text, info).
-spec event(event_title(), event_text(), event_type()) -> ok.
event(Title, Text, Type) -> event(Title, Text, Type, normal).
-spec event(event_title(), event_text(), event_type(), event_priority()) -> ok.
event(Title, Text, Type, Priority) -> event(Title, Text, Type, Priority, #{}).
-spec event(event_title(), event_text(), event_type(), event_priority(), event_tags()) -> ok.
event(Title, Text, Type, Priority, Tags) ->
    send_event(Title, Text, Type, Priority, Tags).

%%%===================================================================
%%% Internal functions
%%%===================================================================

normalize_metric_data({Name, Value}) ->
    {Name, Value, 1.0, #{}};
normalize_metric_data({Name, Value, SampleRate}) when is_number(SampleRate) ->
    {Name, Value, SampleRate, #{}};
normalize_metric_data({Name, Value, Tags}) when is_map(Tags) ->
    {Name, Value, 1.0, #{}};
normalize_metric_data({_Name, _Value, _SampleRate, _Tags} = AlreadyNormalized) ->
    AlreadyNormalized.

%%% Tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

test_setup() ->
    meck:new(wpool),
    meck:expect(wpool, 'cast', fun (dogstatsd_worker, _Data) -> ok end).

test_teardown(_) ->
    meck:unload(wpool).

gauge_test_() ->
    {setup,
     fun test_setup/0,
     fun test_teardown/1,
     [
      ?_assertEqual(ok, dogstatsd:gauge("foo.bar", 1))
     ,?_assertEqual(ok, dogstatsd:gauge("foo.bar", 1, 0.5))
     ,?_assertEqual(ok, dogstatsd:gauge("foo.bar", 1, #{baz => qux}))
     ,?_assertEqual(ok, dogstatsd:gauge("foo.bar", 1, 0.25, #{baz => qux}))
     ,?_assertError(function_clause, dogstatsd:gauge("foo.bar", #{baz => qux}))
     ,?_assertError(function_clause, dogstatsd:gauge("foo.bar", #{baz => qux}, 0.5))
     ,?_assertError(function_clause, dogstatsd:gauge("foo.bar", 1, "hello"))
     ,?_assertEqual(ok, dogstatsd:gauge([{"foo.bar", 1, 0.5, #{foo => bar}},
                                         {"foo.bar", 1, 0.5, #{foo => bar}}]))
     ,?_assertError(function_clause, dogstatsd:gauge([{"foo.bar", 1, 0.5, #{foo => bar}},
                                                      {"foo.bar", 1, "hello"}]))
     ]}.

-endif.
