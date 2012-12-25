-module(neural).

-export([new/1, insert/2, fetch/2, delete/2, empty/1, dump/1, garbage/1, increment/3, unshift/3, shift/3]).
-on_load(init/0).

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

new(Table) ->
    make_table(Table),
    neural_gc_sup:start_child(Table),
    ok.

make_table(_Table) ->
    ?nif_stub.

insert(Table, Object) when is_atom(Table) andalso is_tuple(Object) ->
    insert(Table, erlang:phash2(element(1, Object)), Object).

insert(_Table, _Key, _Object) ->
    ?nif_stub.

increment(Table, Key, Value) when is_integer(Value) ->
    [N] = increment(Table, Key, [{2, Value}]),
    N;
increment(Table, Key, Op = {Position, Value}) when is_integer(Position), is_integer(Value) ->
    [N] = increment(Table, Key, [Op]),
    N;
increment(Table, Key, Op = [_|_]) ->
    case lists:all(fun is_incr_op/1, Op) of
        true ->
            lists:reverse(do_increment(Table, erlang:phash2(Key), Op));
        false ->
            error(badarg)
    end.

shift(Table, Key, Value) when is_integer(Value) ->
    [R] = shift(Table, Key, [{2, Value}]),
    R;
shift(Table, Key, Op = {Position, Value}) when is_integer(Position), is_integer(Value) ->
    [R] = shift(Table, Key, [Op]),
    R;
shift(Table, Key, Op = [_|_]) ->
    case lists:all(fun is_shift_op/1, Op) of
        true ->
            lists:reverse(do_shift(Table, erlang:phash2(Key), Op));
        false ->
            error(badarg)
    end.

unshift(Table, Key, Op = {Position, Value}) when is_integer(Position), is_list(Value) ->
    [R] = unshift(Table, Key, [Op]),
    R;
unshift(Table, Key, Op = [_|_]) ->
    case lists:all(fun is_unshift_op/1, Op) of
        true ->
            lists:reverse(do_unshift(Table, erlang:phash2(Key), Op));
        false ->
            error(badarg)
    end.

is_incr_op({P,V}) when is_integer(P), is_integer(V) -> true;
is_incr_op(_) -> false.

is_shift_op({P,V}) when is_integer(P), is_integer(V) -> true;
is_shift_op(_) -> false.

is_unshift_op({P,L}) when is_integer(P), is_list(L) -> true;
is_unshift_op(_) -> false.

do_increment(_Table, _Key, _Op) ->
    ?nif_stub.

do_shift(_Table, _Key, _Op) ->
    ?nif_stub.

do_unshift(_Table, _Key, _Op) ->
    ?nif_stub.

fetch(Table, Key) ->
    do_fetch(Table, erlang:phash2(Key)).

do_fetch(_Table, _Key) ->
    ?nif_stub.

delete(Table, Key) ->
    do_delete(Table, erlang:phash2(Key)).

do_delete(_Table, _key) -> 
    ?nif_stub.

empty(_Table) ->
    ?nif_stub.

garbage(_Table) ->
    ?nif_stub.

dump(_Table) ->
    ?nif_stub.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    {ok, Ref} = new(),
    ?assertEqual(ok, myfunction(Ref)).

-endif.
