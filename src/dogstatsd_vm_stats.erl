%%% This file is almost entirely copied from Fred Hebert's vmstats project
%%% It's therefore covered under the following license:

%%% ------------------------------------------------------------------------------

%%% Copyright (c) 2012, BLOOM Digital Platforms, Frederic Trottier-Hebert
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%    * Redistributions of source code must retain the above copyright
%%%      notice, this list of conditions and the following disclaimer.
%%%    * Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%    * Neither the name of the BLOOM Digital Platforms nor the
%%%      names of its contributors may be used to endorse or promote products
%%%      derived from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL BLOOM DIGITAL PLATFORMS BE LIABLE FOR ANY
%%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%%% ------------------------------------------------------------------------------

%%% Main worker for vmstats. This module sits in a loop fired off with
%%% timers with the main objective of routinely sending data to
%%% statsderl.
-module(dogstatsd_vm_stats).

-behaviour(gen_server).
%% Interface
-export([start_link/0, start_link/1]).
%% Internal Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-define(TIMER_MSG, '#delay').

-record(state, {key :: string(),
                sched_time :: enabled | disabled | unavailable,
                prev_sched :: [{integer(), integer(), integer()}],
                timer_ref :: reference(),
                delay :: integer(), % milliseconds
                prev_io :: {In::integer(), Out::integer()},
                prev_gc :: {GCs::integer(), Words::integer(), 0}}).
%%% INTERFACE
start_link() ->
    start_link(base_key()).

%% the base key is passed from the supervisor. This function
%% should not be called manually.
start_link(BaseKey) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, BaseKey, []).

%%% INTERNAL EXPORTS
init(BaseKey) ->
    Delay = stillir:get_config(dogstatsd, vm_stats_delay),
    Ref = erlang:start_timer(Delay, self(), ?TIMER_MSG),
    {{input,In},{output,Out}} = erlang:statistics(io),
    PrevGC = erlang:statistics(garbage_collection),
    case {sched_time_available(), stillir:get_config(dogstatsd, vm_stats_scheduler)} of
        {true, true} ->
            {ok, #state{key = [BaseKey,$.],
                        timer_ref = Ref,
                        delay = Delay,
                        sched_time = enabled,
                        prev_sched = lists:sort(erlang:statistics(scheduler_wall_time)),
                        prev_io = {In,Out},
                        prev_gc = PrevGC}};
        {true, _} ->
            {ok, #state{key = [BaseKey,$.],
                        timer_ref = Ref,
                        delay = Delay,
                        sched_time = disabled,
                        prev_io = {In,Out},
                        prev_gc = PrevGC}};
        {false, _} ->
            {ok, #state{key = [BaseKey,$.],
                        timer_ref = Ref,
                        delay = Delay,
                        sched_time = unavailable,
                        prev_io = {In,Out},
                        prev_gc = PrevGC}}
    end.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, R, ?TIMER_MSG}, S = #state{key=K, delay=D, timer_ref=R}) ->
    %% Processes
    dogstatsd:gauge([K,"proc_count"], erlang:system_info(process_count), 1.00),
    dogstatsd:gauge([K,"proc_limit"], erlang:system_info(process_limit), 1.00),

    %% Messages in queues
    TotalMessages = lists:foldl(
        fun(Pid, Acc) ->
            case process_info(Pid, message_queue_len) of
                undefined -> Acc;
                {message_queue_len, Count} -> Count+Acc
            end
        end,
        0,
        processes()
    ),
    dogstatsd:gauge([K,"messages_in_queues"], TotalMessages, 1.00),

    %% Modules loaded
    dogstatsd:gauge([K,"modules"], length(code:all_loaded()), 1.00),

    %% Queued up processes (lower is better)
    dogstatsd:gauge([K,"run_queue"], erlang:statistics(run_queue), 1.00),

    %% Error logger backlog (lower is better)

    case whereis(error_logger) of
        undefined -> undefined;
        Pid ->
            {_, MQL} = process_info(Pid, message_queue_len),
            dogstatsd:gauge([K,"error_logger_queue_len"], MQL, 1.00)
    end,

    %% Memory usage. There are more options available, but not all were kept.
    %% Memory usage is in bytes.
    K2 = [K,"memory."],
    Mem = erlang:memory(),
    dogstatsd:gauge([K2,"total"], proplists:get_value(total, Mem), 1.00),
    dogstatsd:gauge([K2,"procs_used"], proplists:get_value(processes_used,Mem), 1.00),
    dogstatsd:gauge([K2,"atom_used"], proplists:get_value(atom_used,Mem), 1.00),
    dogstatsd:gauge([K2,"binary"], proplists:get_value(binary, Mem), 1.00),
    dogstatsd:gauge([K2,"ets"], proplists:get_value(ets, Mem), 1.00),

    %% Incremental values
    #state{prev_io={OldIn,OldOut}, prev_gc={OldGCs,OldWords,_}} = S,
    {{input,In},{output,Out}} = erlang:statistics(io),
    GC = {GCs, Words, _} = erlang:statistics(garbage_collection),

    dogstatsd:increment([K,"io.bytes_in"], In-OldIn, 1.00),
    dogstatsd:increment([K,"io.bytes_out"], Out-OldOut, 1.00),
    dogstatsd:increment([K,"gc.count"], GCs-OldGCs, 1.00),
    dogstatsd:increment([K,"gc.words_reclaimed"], Words-OldWords, 1.00),

    %% Reductions across the VM, excluding current time slice, already incremental
    {_, Reds} = erlang:statistics(reductions),
    dogstatsd:increment([K,"reductions"], Reds, 1.00),

    %% Scheduler wall time
    #state{sched_time=Sched, prev_sched=PrevSched} = S,
    case Sched of
        enabled ->
            NewSched = lists:sort(erlang:statistics(scheduler_wall_time)),
            [begin
                SSid = integer_to_list(Sid),
                dogstatsd:timing([K,"scheduler_wall_time.active"], Active, 1.00, #{scheduler => SSid}),
                dogstatsd:timing([K,"scheduler_wall_time.total"], Total, 1.00, #{scheduler => SSid})
             end
             || {Sid, Active, Total} <- wall_time_diff(PrevSched, NewSched)],
            {noreply, S#state{timer_ref=erlang:start_timer(D, self(), ?TIMER_MSG),
                              prev_sched=NewSched, prev_io={In,Out}, prev_gc=GC}};
        _ -> % disabled or unavailable
            {noreply, S#state{timer_ref=erlang:start_timer(D, self(), ?TIMER_MSG),
                              prev_io={In,Out}, prev_gc=GC}}
    end;
handle_info(_Msg, {state, _Key, _TimerRef, _Delay}) ->
    exit(forced_upgrade_restart);
handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% Returns the two timeslices as a ratio of each other,
%% as a percentage so that StatsD gets to print something > 1
wall_time_diff(T1, T2) ->
    [{I, Active2-Active1, Total2-Total1}
     || {{I, Active1, Total1}, {I, Active2, Total2}} <- lists:zip(T1,T2)].

sched_time_available() ->
    try erlang:system_flag(scheduler_wall_time, true) of
        _ -> true
    catch
        error:badarg -> false
    end.

-spec base_key() -> term().
base_key() ->
    stillir:get_config(dogstatsd, vm_stats_base_key).
