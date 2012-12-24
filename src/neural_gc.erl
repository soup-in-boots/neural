%% ================================================================
%% @author          Zachary Hueras
%% @copyright       2012 Zachary Hueras
%% @version         0.0.1
%% @doc             Perform garbage collection for a given table.
%% ================================================================
-module(neural_gc).
-behaviour(gen_server).
-export([start_link/1]).
-export([collect_garbage/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(gc_state, {
        tid = undefined :: atom()
    }).

collect_garbage(Pid) ->
    gen_server:cast(Pid, collect_garbage).

start_link(Table) ->
    gen_server:start_link(?MODULE, [Table], []).

init([Table]) ->
    io:format("Garbage collector monitoring table ~w~n", [Table]),
    timer:apply_interval(1000, ?MODULE, collect_garbage, [self()]),
    {ok, #gc_state{tid = Table}}.

handle_call(_Call, _From, State) ->
    {reply, undefined, State}.

handle_cast(collect_garbage, State = #gc_state{tid = Tid}) ->
    neural:garbage(Tid),
    {noreply, State};
handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
