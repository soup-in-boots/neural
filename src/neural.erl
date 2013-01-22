-module(neural).

-export([new/2, empty/1, drain/1, dump/1,       % Table operations
         garbage/1, garbage_size/1, 
         key_pos/1]).
-export([lookup/2]).                            % Getters
-export([insert/2, insert_new/2, delete/2]).    % Setters
-export([increment/3, unshift/3, shift/3, swap/3]).     % Delta operations
-on_load(init/0).
-record(table_opts, {
        keypos      = 1 :: integer()
    }).

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

new(Table, Opts) ->
    new(Table, Opts, #table_opts{}).

new(Table, [{key_pos, KeyPos}|Opts], TableOpts) ->
    new(Table, Opts, TableOpts#table_opts{keypos = KeyPos});
new(Table, [], _TableOpts = #table_opts{keypos = KeyPos}) when is_integer(KeyPos) ->
    make_table(Table, KeyPos).

make_table(_Table, _KeyPos) ->
    ?nif_stub.

insert(Table, Object) when is_atom(Table), is_tuple(Object) ->
    Key = element(key_pos(Table), Object),
    insert(Table, erlang:phash2(Key), Object).

insert(_Table, _Key, _Object) ->
    ?nif_stub.

insert_new(Table, Object) when is_atom(Table), is_tuple(Object) ->
    Key = element(key_pos(Table), Object),
    insert_new(Table, erlang:phash2(Key), Object).

insert_new(_Table, _Key, _Object) ->
    ?nif_stub.

increment(Table, Key, Value) when is_integer(Value) ->
    [N] = increment(Table, Key, [{key_pos(Table) + 1, Value}]),
    N;
increment(Table, Key, Op = {Position, Value}) when is_integer(Position), is_integer(Value) ->
    [N] = increment(Table, Key, [Op]),
    N;
increment(Table, Key, Op = [_|_]) when is_atom(Table) ->
    case lists:all(fun is_incr_op/1, Op) of
        true ->
            lists:reverse(do_increment(Table, erlang:phash2(Key), Op));
        false ->
            error(badarg)
    end.

shift(Table, Key, Value) when is_integer(Value) ->
    [R] = shift(Table, Key, [{key_pos(Table) + 1, Value}]),
    R;
shift(Table, Key, Op = {Position, Value}) when is_integer(Position), is_integer(Value) ->
    [R] = shift(Table, Key, [Op]),
    R;
shift(Table, Key, Op = [_|_]) when is_atom(Table) ->
    case lists:all(fun is_shift_op/1, Op) of
        true ->
            lists:reverse(do_shift(Table, erlang:phash2(Key), Op));
        false ->
            error(badarg)
    end.

unshift(Table, Key, Op = {Position, Value}) when is_integer(Position), is_list(Value) ->
    [R] = unshift(Table, Key, [Op]),
    R;
unshift(Table, Key, Op = [_|_]) when is_atom(Table) ->
    case lists:all(fun is_unshift_op/1, Op) of
        true ->
            lists:reverse(do_unshift(Table, erlang:phash2(Key), Op), []);
        false ->
            error(badarg)
    end.

swap(Table, Key, Op = {Position, _Value}) when is_integer(Position) ->
    [R] = swap(Table, Key, [Op]),
    R;
swap(Table, Key, Op = [_|_]) when is_atom(Table) ->
    case lists:all(fun is_swap_op/1, Op) of 
        true ->
            lists:reverse(do_swap(Table, erlang:phash2(Key), Op));
        false ->
            error(badarg)
    end.

is_incr_op({P,V}) when is_integer(P), is_integer(V) -> true;
is_incr_op(_) -> false.

is_shift_op({P,V}) when is_integer(P), is_integer(V) -> true;
is_shift_op(_) -> false.

is_unshift_op({P,L}) when is_integer(P), is_list(L) -> true;
is_unshift_op(_) -> false.

is_swap_op({P,V}) when is_integer(P) -> true;
is_swap_op(_) -> false.

do_increment(_Table, _Key, _Op) ->
    ?nif_stub.

do_shift(_Table, _Key, _Op) ->
    ?nif_stub.

do_unshift(_Table, _Key, _Op) ->
    ?nif_stub.

do_swap(_Table, _Key, _Op) ->
    ?nif_stub.

lookup(Table, Key) when is_atom(Table) ->
    do_fetch(Table, erlang:phash2(Key)).

do_fetch(_Table, _Key) ->
    ?nif_stub.

delete(Table, Key) when is_atom(Table) ->
    do_delete(Table, erlang:phash2(Key)).

do_delete(_Table, _key) -> 
    ?nif_stub.

garbage(_Table) ->
    ?nif_stub.

garbage_size(_Table) ->
    ?nif_stub.

empty(_Table) ->
    ?nif_stub.

drain(Table) ->
    '$neural_batch_wait' = do_drain(Table),
    wait_batch_response().

do_drain(_Table) ->
    ?nif_stub.

dump(Table) ->
    '$neural_batch_wait' = do_dump(Table),
    wait_batch_response().

do_dump(_Table) ->
    ?nif_stub.

key_pos(_Table) ->
    ?nif_stub.

wait_batch_response() ->
    receive
        {'$neural_batch_response', Response} -> Response
    end.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    {ok, Ref} = new(),
    ?assertEqual(ok, myfunction(Ref)).

-endif.
