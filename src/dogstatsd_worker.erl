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

handle_cast(Data, State) ->
    Line = build_line(Data, State),
    send_line(Line, State),
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
build_line({Type, Name, Value, SampleRate, Tags}, #state{prefix="", tags=GlobalTags}) ->
    Tags1 = maps:merge(GlobalTags, Tags),
    build_line_helper({Type, Name, Value, SampleRate, Tags1});
build_line({Type, Name, Value, SampleRate, Tags}, #state{prefix=GlobalPrefix, tags=GlobalTags}) ->
    Tags1 = maps:merge(GlobalTags, Tags),
    Name1 = [GlobalPrefix, $., Name],
    build_line_helper({Type, Name1, Value, SampleRate, Tags1}).

build_line_helper({Type, Name, Value, SampleRate, Tags}) ->
    LineStart = io_lib:format("~s:~b|~s|@~.2f", [Name, Value, type_to_str(Type), SampleRate]),
    TagLine = maps:fold(fun (Key, Val, []) ->
                                ["|#", kv(Key, Val)];
                            (Key, Val, Acc) ->
                                [Acc, ",", kv(Key, Val)]
                        end,
                        [],
                        Tags),
    [LineStart, TagLine].

send_line(_, #state{socket = no_send}) ->
    ok;
send_line(Line, #state{socket = Socket, host = Host, port = Port}) ->
    ok = gen_udp:send(Socket, Host, Port, Line).

type_to_str(counter) -> "c";
type_to_str(gauge) -> "g";
type_to_str(histogram) -> "h";
type_to_str(timer) -> "ms";
type_to_str(set) -> "s".

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
