-module(dogstatsd).

-type metric_name() :: iodata().
-type metric_value() :: number().
-type metric_type() :: counter | gauge | histogram | timer | set.
-type metric_sample_rate() :: number().
-type metric_tags() :: map().

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
             ]).

-export([
        gauge/2, gauge/3, gauge/4
        ,counter/2, counter/3, counter/4
        ,increment/2, increment/3, increment/4
        ,histogram/2, histogram/3, histogram/4
        ,timer/2, timer/3, timer/4
        ,timing/2, timing/3, timing/4
        ,set/2, set/3, set/4
        ,event/1, event/2, event/3, event/4, event/5
        ]).

-spec send_metric(metric_type(), metric_name(), metric_value(), metric_sample_rate(), metric_tags()) -> ok.
send_metric(Type, Name, Value, SampleRate, Tags) ->
    send({metric, {Type, Name, Value, SampleRate, Tags}}).

-spec send_event(event_title(), event_text(), event_type(), event_priority(), event_tags()) -> ok.
send_event(Title, Text, Type, Priority, Tags) ->
    send({event, {Title, Text, Type, Priority, Tags}}).

-spec send({atom(), tuple()}) -> ok.
send(Data) ->
    wpool:cast(dogstatsd_worker, Data).

-define(SPEC_TYPE_2(Type), -spec Type(metric_name(), metric_value()) -> ok).
-define(MK_TYPE_2(Type),
        Type(Name, Value) ->
               send_metric(Type, Name, Value, 1.0, #{})
).
-define(SPEC_TYPE_3(Type), -spec Type(metric_name(), metric_value(), metric_sample_rate()|metric_tags()) -> ok).
-define(MK_TYPE_3(Type),
        Type(Name, Value, SampleRate) when is_number(SampleRate) ->
               send_metric(Type, Name, Value, SampleRate, #{});
        Type(Name, Value, Tags) when is_map(Tags) ->
               send_metric(Type, Name, Value, 1.0, Tags)
).
-define(SPEC_TYPE_4(Type), -spec Type(metric_name(), metric_value(), metric_sample_rate(), metric_tags()) -> ok).
-define(MK_TYPE_4(Type),
        Type(Name, Value, SampleRate, Tags) when is_number(SampleRate), is_map(Tags) ->
               send_metric(Type, Name, Value, SampleRate, Tags)
).

-define(ALIAS_TYPE_2(Alias, Real), Alias(A, B) -> Real(A, B)).
-define(ALIAS_TYPE_3(Alias, Real), Alias(A, B, C) -> Real(A, B, C)).
-define(ALIAS_TYPE_4(Alias, Real), Alias(A, B, C, D) -> Real(A, B, C, D)).

?SPEC_TYPE_2(gauge).
?SPEC_TYPE_3(gauge).
?SPEC_TYPE_4(gauge).
?MK_TYPE_2(gauge).
?MK_TYPE_3(gauge).
?MK_TYPE_4(gauge).

?SPEC_TYPE_2(counter).
?SPEC_TYPE_3(counter).
?SPEC_TYPE_4(counter).
?MK_TYPE_2(counter).
?MK_TYPE_3(counter).
?MK_TYPE_4(counter).
?ALIAS_TYPE_2(increment, counter).
?ALIAS_TYPE_3(increment, counter).
?ALIAS_TYPE_4(increment, counter).

?SPEC_TYPE_2(histogram).
?SPEC_TYPE_3(histogram).
?SPEC_TYPE_4(histogram).
?MK_TYPE_2(histogram).
?MK_TYPE_3(histogram).
?MK_TYPE_4(histogram).

?SPEC_TYPE_2(timer).
?SPEC_TYPE_3(timer).
?SPEC_TYPE_4(timer).
?MK_TYPE_2(timer).
?MK_TYPE_3(timer).
?MK_TYPE_4(timer).
?ALIAS_TYPE_2(timing, timer).
?ALIAS_TYPE_3(timing, timer).
?ALIAS_TYPE_4(timing, timer).

?SPEC_TYPE_2(set).
?SPEC_TYPE_3(set).
?SPEC_TYPE_4(set).
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
