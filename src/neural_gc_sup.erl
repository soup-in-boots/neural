-module(neural_gc_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([start_child/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Table) ->
    supervisor:start_child(?MODULE, [Table]).

init([]) ->
    {ok, {
            {simple_one_for_one, 5, 10},
            [
                {
                    neural_gc,
                    {neural_gc, start_link, []},
                    permanent,
                    5000,
                    worker,
                    [neural_gc]
                }
            ]
        }}.
