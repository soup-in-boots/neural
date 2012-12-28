-module(neural_concurrency).
-compile([{parse_transform, ct_expand}]).
-export([test/0]).
-define(KEYS, ct_expand:term([ {"test_key", N} || N <- lists:seq(1, 1000) ])).
-define(NUM_KEYS, 1000).

test() ->
    neural:new(test, []),
    io:format("Insert time: ~p~n", [begin {Dur, _} = timer:tc(fun() -> [ neural:insert(test, {Key, 0, 0, 0}) || Key <- ?KEYS ] end), Dur end]),
    Pids = [ spawn(fun neural_worker/0) || _ <- lists:seq(1,64) ],
    Refs = lists:flatten([ [ begin Ref = make_ref(), Pid ! {go, self(), Ref}, Ref end || Pid <- Pids ] || _ <- lists:seq(1, 1000) ]),
    io:format("Generated ~p requests.~n", [length(Refs)]),
    wait(Pids, Refs).

wait(Pids, Refs) ->
    wait(Pids, Refs, []).

wait(Pids, [], Durations) ->
    [ exit(Pid, normal) || Pid <- Pids ],
    Sorted = lists:sort(Durations),
    Min = lists:min(Durations),
    Max = lists:max(Durations),
    Cnt = length(Durations),
    Nth = trunc(Cnt * 0.9),
    U90 = lists:sublist(Sorted, Nth),
    Avg = lists:sum(U90) / Nth,
    Percentile = lists:nth(Nth, U90),
    Res = neural:dump(test),
    io:format("Min: ~p; Max: ~p: Avg: ~p; U90: ~p; Res: ~n", [Min, Max, Avg, Percentile]),
    [ io:format("~p~n", [Val]) || Val <- Res ],
    ok;
wait(Pids, Refs, Durations) ->
    receive
        {done, Ref, Dur} -> 
            case crypto:rand_uniform(1, 1000) of
                1 -> io:format("Refs remaining: ~p~n", [length(Refs)]);
                _ -> ok
            end,
            wait(Pids, lists:delete(Ref, Refs), [Dur|Durations])
    end.

neural_worker() ->
    receive
        {go, Pid, Ref} ->
            Key = lists:nth(crypto:rand_uniform(1, ?NUM_KEYS + 1), ?KEYS),
            {Dur, _} = timer:tc(fun() -> neural:increment(test, Key, [{2, 1}, {3, 10}, {4, 100}]) end),
            Pid ! {done, Ref, Dur},
            neural_worker()
    end.
