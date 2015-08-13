%% @doc Key-Value store to lookup a cell process pid starting
%% from the position of the cell.
%%
%% This is used as an anti-corruption layer because the rest of
%% the application doesn't have to care about cell's pids (because
%% they can vary with time) but can refer to the cells using their
%% positions, which are stable for all the lifetime of the application.
-module(cell_locator).
-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").

-export([start_link/0,
         stop/0,
         put/2,
         get/1,
         wait_for/2,
         wait_for_all/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {pids,
                positions}).

%% API functions

%% @doc Starts a cell_locator process and registers it locally.
-spec start_link() -> {ok, pid()} |
                      ignore |
                      {error, already_started} |
                      {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Inserts or updates the pid for a cell position.
-spec put(Pos :: cell:position(), Pid :: pid()) -> ok.
put(Pos, Pid) ->
    gen_server:call(?SERVER, {put, Pos, Pid}).

%% @doc Gets the pid for a cell position.
-spec get(Pos :: cell:position()) -> pid() | {error, not_found}.
get(Pos) ->
    gen_server:call(?SERVER, {get, Pos}).

%% @doc Stops the cell_locator.
-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER, stop).

%% @doc Waits for a cell to be registered on the cell_locator process.
%% It waits until the timeout (in milliseconds) is reached or the cell
%% is registered.
-spec wait_for(Pos :: cell:position(), T :: timeout()) -> ok | timeout.
wait_for(_, T) when T =< 0 ->
    timeout;
wait_for(Pos, T) ->
    case cell_locator:get(Pos) of
        {error, not_found} ->
            timer:sleep(10),
            cell_locator:wait_for(Pos, T - 10);
        Pid when is_pid(Pid) ->
            ok
    end.

%% @doc Waits for all the cells in the list to be registered on the cell_locator
%% process.
%% It waits until the timeout (in milliseconds) is reached or all the cells are
%% registered.
-spec wait_for_all(PosList :: [cell:position(), ...], T :: timeout()) ->
                          ok | timeout.
wait_for_all([], _) ->
    ok;
wait_for_all([CellPos|OtherPos], T) ->
    case cell_locator:wait_for(CellPos, T) of
        timeout ->
            timeout;
        ok ->
            cell_locator:wait_for_all(OtherPos, T)
    end.

%% gen_server callbacks

init([]) ->
    {ok, #state{pids=gb_trees:empty(),
                positions=gb_trees:empty()}}.

handle_call({get, Pos}, _From, S) ->
    case gb_trees:lookup(Pos, S#state.pids) of
        {value, Value} ->
            {reply, Value, S};
        none ->
            {reply, {error, not_found}, S}
    end;
handle_call({put, Pos, Pid}, _From, S) ->
    erlang:monitor(process, Pid),
    Pids = gb_trees:enter(Pos, Pid, S#state.pids),
    Positions = gb_trees:enter(Pid, Pos, S#state.positions),
    {reply, ok, S#state{pids=Pids,
                        positions=Positions}};
handle_call(stop, From, S) ->
    gen_server:reply(From, ok),
    {stop, normal, S}.

handle_cast(_Req, S) ->
    {noreply, S}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, S) ->
    case gb_trees:lookup(Pid, S#state.positions) of
        {value, Pos} ->
            Pids = gb_trees:delete(Pos, S#state.pids),
            Positions = gb_trees:delete(Pid, S#state.positions),
            gen_event:notify(deb, {cell_died, Pos}),
            {noreply, #state{pids=Pids,
                             positions=Positions}};
        none ->
            {noreply, S}
    end.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%% tests
% -ifdef(TEST).

% all_tests_test_() ->
%     {inorder, {foreach,
%                fun setup_locator/0,
%                fun teardown_locator/1,
%                [
%                 fun cell_not_found/0,
%                 fun cell_found/0,
%                 fun cell_update/0,
%                 fun cell_location_is_removed_when_monitored_process_goes_down/0
%                ]}}.

% setup_locator() ->
%     meck:new(dgol),
%     meck:expect(dgol, target_time, fun() -> 0 end),
%     gen_event:start_link({local, deb}),
%     cell_locator:start_link().

% teardown_locator(_) ->
%     meck:unload(dgol),
%     gen_event:stop(deb),
%     cell_locator:stop().

% cell_not_found() ->
%     ?assertEqual({error, not_found}, cell_locator:get({1, 2})).

% cell_found() ->
%     {ok, CellPid} = cell:start_link({1, 2}, {3, 3}, 1),
%     cell_locator:put({1, 2}, CellPid),
%     ?assertEqual(CellPid, cell_locator:get({1, 2})).

% cell_update() ->
%     {ok, CellPid} = cell:start_link({1, 2}, {3, 3}, 1),
%     ?assertEqual(ok, cell_locator:put({1, 2}, CellPid)),
%     ?assertEqual(ok, cell_locator:put({1, 2}, CellPid)).

% cell_location_is_removed_when_monitored_process_goes_down() ->
%     process_flag(trap_exit, true),
%     {ok, CellPid} = cell:start_link({1, 2}, {3, 3}, 1),
%     ?assertEqual(ok, cell_locator:put({1, 2}, CellPid)),
%     exit(CellPid, kill),
%     ?assertNot(erlang:is_process_alive(CellPid)),
%     ?assertEqual({error, not_found}, cell_locator:get({1, 2})).

% -endif.
