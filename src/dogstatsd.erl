-module(dogstatsd).

-type metric_name() :: iodata().
-type metric_value() :: number().
-type metric_type() :: counter | gauge | histogram | timer | set.
-type metric_sample_rate() :: number().
-type metric_tags() :: map().

-export_type([
              metric_name/0
             ,metric_value/0
             ,metric_type/0
             ,metric_sample_rate/0
             ,metric_tags/0
             ]).

-export([
         send/5
        ,gauge/2, gauge/3, gauge/4
        ,counter/2, counter/3, counter/4
        ,histogram/2, histogram/3, histogram/4
        ,timer/2, timer/3, timer/4
        ,set/2, set/3, set/4
        ]).


-spec send(metric_type(), metric_name(), metric_value(), metric_sample_rate(), metric_tags()) -> ok.
send(Type, Name, Value, SampleRate, Tags) ->
    wpool:cast(dogstatsd_worker, {Type, Name, Value, SampleRate, Tags}).


-define(SPEC_TYPE_2(Type), -spec Type(metric_name(), metric_value()) -> ok).
-define(MK_TYPE_2(Type),
        Type(Name, Value) ->
               send(Type, Name, Value, 1.0, #{})
).
-define(SPEC_TYPE_3(Type), -spec Type(metric_name(), metric_value(), metric_sample_rate()|metric_tags()) -> ok).
-define(MK_TYPE_3(Type),
        Type(Name, Value, SampleRate) when is_number(SampleRate) ->
               send(Type, Name, Value, SampleRate, #{});
        Type(Name, Value, Tags) when is_map(Tags) ->
               send(Type, Name, Value, 1.0, Tags)
).
-define(SPEC_TYPE_4(Type), -spec Type(metric_name(), metric_value(), metric_sample_rate(), metric_tags()) -> ok).
-define(MK_TYPE_4(Type),
        Type(Name, Value, SampleRate, Tags) when is_number(SampleRate), is_map(Tags) ->
               send(Type, Name, Value, SampleRate, Tags)
).

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

?SPEC_TYPE_2(set).
?SPEC_TYPE_3(set).
?SPEC_TYPE_4(set).
?MK_TYPE_2(set).
?MK_TYPE_3(set).
?MK_TYPE_4(set).
