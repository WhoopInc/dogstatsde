-module(dogstatsd_sup).
-behavior(supervisor).
-export([init/1]).
-export([start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    Children = [
                #{
                   id => dogstatsd_worker_pool,
                   start => {wpool, start_pool, [dogstatsd_worker, [{worker, {dogstatsd_worker, []}}, {workers, 10}]]},
                   restart => permanent,
                   shutdown => infinity,
                   type => supervisor,
                   modules => [dogstatsd_worker]
                 } |
                case stillir:get_config(dogstatsd, vm_stats) of
                    true ->
                        [#{
                            id => dogstatsd_vm_stats,
                            start => {dogstatsd_vm_stats, start_link, []},
                            restart => permanent,
                            shutdown => brutal_kill
                          }];
                    false ->
                        []
                end
               ],
    SupFlags = #{
      strategy => one_for_one,
      intensity => 5,
      period => 60
     },
    {ok, {SupFlags, Children}}.
