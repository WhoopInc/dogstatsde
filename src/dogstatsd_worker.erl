%%% @author Nathaniel Waisbrot <waisbrot@whoop.com>
-module(dogstatsd_worker).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          socket,
          host,
          port,
          prefix,
          tags
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    State = case stillir:get_config(dogstatsd, send_metrics) of
                true ->
                    {ok, Socket} = gen_udp:open(0),
                    #state{
                       socket = Socket,
                       host = stillir:get_config(dogstatsd, agent_address),
                       port = stillir:get_config(dogstatsd, agent_port),
                       prefix = stillir:get_config(dogstatsd, global_prefix),
                       tags = stillir:get_config(dogstatsd, global_tags)
                      };
                false ->
                    #state{socket = no_send}
            end,
    {ok, State}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Data, #state{socket = no_send} = State) ->
    {noreply, State};
handle_cast(Data, State) ->
    Line = build_line(Data, State),
    ok = send_line(Line, State),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

build_line({metric, Data}, State) ->
    build_metric_line(Data, State);
build_line({event, Data}, State) ->
    build_event_line(Data, State).

build_metric_line({Type, Name, Value, SampleRate, Tags}, State) ->
    LineStart = io_lib:format("~s:~.3f|~s|@~.2f", [prepend_global_prefix(Name, State), float(Value),
                                                    metric_type_to_str(Type), float(SampleRate)]),
    TagLine = build_tag_line(Tags, State),
    [LineStart, TagLine].

prepend_global_prefix(Name, #state{prefix=""}) -> Name;
prepend_global_prefix(Name, #state{prefix=GlobalPrefix}) -> [GlobalPrefix, $., Name].

build_event_line({Title, Text, Type, Priority, Tags}, State) ->
    TitleBin = iodata_to_bin(Title),
    TextBin = iodata_to_bin(Text),
    TitleLen = byte_size(TitleBin),
    TextLen = byte_size(TextBin),
    LineStart = io_lib:format("_e{~b,~b}:~s|~s|t:~s|p:~s", [TitleLen, TextLen, TitleBin,
                                                            TextBin, Type, Priority]),
    TagLine = build_tag_line(Tags, State),
    [LineStart, TagLine].

build_tag_line(Tags, #state{tags=GlobalTags}) ->
    maps:fold(fun (Key, Val, []) ->
                      ["|#", kv(Key, Val)];
                  (Key, Val, Acc) ->
                      [Acc, ",", kv(Key, Val)]
              end,
              [],
              maps:merge(GlobalTags, Tags)).

send_line(Line, #state{socket = Socket, host = Host, port = Port}) ->
    ok = gen_udp:send(Socket, Host, Port, Line).

metric_type_to_str(counter) -> "c";
metric_type_to_str(gauge) -> "g";
metric_type_to_str(histogram) -> "h";
metric_type_to_str(timer) -> "ms";
metric_type_to_str(set) -> "s".

kv(K, V) when is_atom(K) ->
    kv(atom_to_list(K), V);
kv(K, V) when is_atom(V) ->
    kv(K, atom_to_list(V));
kv(K, V) when is_number(K) ->
    kv(io_lib:format("~b", [K]), V);
kv(K, V) when is_number(V) ->
    kv(K, io_lib:format("~b", [V]));
kv(K, V) ->
    [K, $:, V].

iodata_to_bin(Bin) when is_binary(Bin) -> Bin;
iodata_to_bin(IoList) -> iolist_to_binary(IoList).

%%% Tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

build_line_test_() ->
    State = #state{
        prefix = "test_global_prefix",
        tags = #{"test" => true}
    },

    [{"for an event",
        begin
            Title1 = ["my ", <<"eve">>, ["nt's title"]],
            Text1 = <<"my event's text">>,
            Type1 = success,
            Priority1 = low,
            Tags1 = #{"event" => "awesome"},

            ExpectedLine1 = <<"_e{16,15}:my event's title|my event's text|t:success|p:low|#event:awesome,test:true">>,
            ActualLine1 = build_line({event, {Title1, Text1, Type1, Priority1, Tags1}}, State),

            ?_assertEqual(ExpectedLine1, iolist_to_binary(ActualLine1))
        end},
      {"for a metric",
        begin
            Type2 = histogram,
            Name2 = ["mymetric_", [<<"name">>]],
            Value2 = 28.0,
            SampleRate2 = 12,
            Tags2 = #{"version" => 42},

            ExpectedLine2 = <<"test_global_prefix.mymetric_name:28.000|h|@12.00|#test:true,version:42">>,
            ActualLine2 = build_line({metric, {Type2, Name2, Value2, SampleRate2, Tags2}}, State),

            ?_assertEqual(ExpectedLine2, iolist_to_binary(ActualLine2))
        end}].

-endif.
