-module(cell_locator).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0
        ,stop/0
        ,put/2
        ,get/1
        ,wait_for/2
        ,wait_for_all/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {pids
               ,positions}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec put(cell:position(), pid()) -> ok.
put(Position, Pid) ->
    gen_server:call(?SERVER, {put, Position, Pid}).

-spec get(cell:position()) -> pid() | {error, not_found}.
get(Position) ->
    gen_server:call(?SERVER, {get, Position}).

-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER, stop).

-spec wait_for(Position :: cell:position(), Timeout :: timeout()) -> ok | timeout.
wait_for(_, Timeout) when Timeout =< 0 ->
    timeout;
wait_for(Position, Timeout) ->
    case cell_locator:get(Position) of
        {error, not_found} -> 
            timer:sleep(10),
            cell_locator:wait_for(Position, Timeout - 10);
        Pid when is_pid(Pid) ->
            ok
    end.

-spec wait_for_all(Positions :: [cell:position(), ...], Timeout :: timeout()) -> ok | timeout.
wait_for_all([], _) ->
    ok;
wait_for_all([H|T], Timeout) ->
    case cell_locator:wait_for(H, Timeout) of
        timeout ->
            timeout;
        ok ->
            cell_locator:wait_for_all(T, Timeout)
    end.

init([]) ->
    {ok, #state{pids=gb_trees:empty()
               ,positions=gb_trees:empty()}}.

handle_call({get, Position}, _From, State) ->
    case gb_trees:lookup(Position, State#state.pids) of
        {value, Value} ->
            {reply, Value, State};
        none ->
            {reply, {error, not_found}, State}
    end;
handle_call({put, Position, Pid}, _From, State) ->
    erlang:monitor(process, Pid),
    UpdatedPids = gb_trees:enter(Position, Pid, State#state.pids),
    UpdatedPositions = gb_trees:enter(Pid, Position, State#state.positions),
    %% TODO: use pacer to update cell
    {reply, ok, State#state{pids=UpdatedPids
                           ,positions=UpdatedPositions}};
handle_call(stop, From, State) ->
    %% synch termination can only be done with a sync reply handled by the 
    %% user
    %% TODO: tell to someone
    gen_server:reply(From, ok),
    {stop, normal, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    case gb_trees:lookup(Pid, State#state.positions) of
        {value, Position} ->
            UpdatedPids = gb_trees:delete(Position, State#state.pids),
            UpdatedPositions = gb_trees:delete(Pid, State#state.positions),
            {noreply, #state{pids=UpdatedPids
                            ,positions=UpdatedPositions}};
        none ->
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-ifdef(TEST).

all_tests_test_() ->
    {inorder, {foreach, 
               fun setup_locator/0,
               fun teardown_locator/1,
               [
                fun cell_not_found/0,
                fun cell_found/0,
                fun cell_update/0,
                fun cell_location_is_removed_when_monitored_process_goes_down/0
               ]}}.

setup_locator() ->
    cell_locator:start_link().

teardown_locator(_) ->
    cell_locator:stop().

cell_not_found() ->
    ?assertEqual({error, not_found}, cell_locator:get({1, 2})).

cell_found() ->
    cell_locator:put({1, 2}, self()),
    ?assertEqual(self(), cell_locator:get({1, 2})).

cell_update() ->
    ?assertEqual(ok, cell_locator:put({1, 2}, self())),
    ?assertEqual(ok, cell_locator:put({1, 2}, self())).

cell_location_is_removed_when_monitored_process_goes_down() ->
    Process = spawn(fun() ->
                            receive
                                _ -> ok
                            end end),
    ?assertEqual(ok, cell_locator:put({1, 2}, Process)),
    exit(Process, kill),
    ?assertNot(erlang:is_process_alive(Process)),
    ?assertEqual({error, not_found}, cell_locator:get({1, 2})).

-endif.
