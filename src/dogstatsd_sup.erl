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
    new_to_old({ok, {SupFlags, Children}}).

new_to_old({ok, {SupFlags, Children}}) ->
    {ok, {new_to_old_supflags(SupFlags), new_to_old_children(Children)}}.
new_to_old_supflags(#{strategy := S, intensity := I, period := P}) ->
    {S, I, P}.
new_to_old_children(Children) ->
    lists:map(fun (Child = #{id := Id, start := Start, restart := Restart, shutdown := Shutdown}) ->
                      Type = maps:get(type, Child, worker),
                      {StartModule,_,_} = Start,
                      Modules = maps:get(modules, Child, StartModule),
                      {Id, Start, Restart, Shutdown, Type, Modules}
              end,
              Children).
