-module(collector).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([start_link/3]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {time :: cell:time(),
                neighbours_coordinates :: cell:neighbours(),
                neighbours_alive :: non_neg_integer(),
                neighbours_monitors :: [{cell:position(), reference()}],
                neighbours_collected :: non_neg_integer(),
                callback :: fun()}).
-define(STARTING_TIMEOUT, 10).

-spec start_link(cell:time(), cell:neighbours(), fun()) -> {ok, pid()}.
start_link(TimeToCollect, Neighbours, Callback) ->
    gen_server:start_link(?MODULE,
                          #state{time=TimeToCollect,
                                 neighbours_coordinates=Neighbours,
                                 neighbours_alive=0,
                                 neighbours_monitors=[],
                                 neighbours_collected=0,
                                 callback=Callback},
                          []).

init(State) ->
    timer:apply_after(10, gen_server, cast, [self(), collect_cells]),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(collect_cells, State) ->
    lists:foreach(fun(Pos) -> collect_cell(self(), Pos) end,
                  State#state.neighbours_coordinates),
    {noreply, State};
handle_cast({collect_cell, Pos}, State) ->
    case cell_locator:get(Pos) of
        {error, not_found} ->
            timer:apply_after(10, gen_server, cast, [self(), {collect_cell, Pos}]),
            {noreply, State};
        Pid when is_pid(Pid) ->
            Self = self(),
            Callback = fun(Result) -> gen_server:cast(Self, Result) end,
            Ref = erlang:monitor(process, Pid),
            cell:eventually_get(Pid, State#state.time, Callback),
            Monitors = [{Pos, Ref}|State#state.neighbours_monitors],
            NewState = State#state{neighbours_monitors=Monitors},
            {noreply, NewState}
    end;
handle_cast({cell, Pos, Time, Content}, State) when Time =:= State#state.time ->
    case lists:keyfind(Pos, 1, State#state.neighbours_monitors) of
        {Pos, Ref} ->
            AliveCount = State#state.neighbours_alive + Content,
            Monitors = lists:subtract(State#state.neighbours_monitors,
                                      [{Pos, Ref}]),
            CollectedCount = State#state.neighbours_collected + 1,

            erlang:demonitor(Ref),
            NewState = State#state{neighbours_monitors=Monitors,
                                   neighbours_alive=AliveCount,
                                   neighbours_collected=CollectedCount},
            case all_collected(NewState) of
                true ->
                    Callback = State#state.callback,
                    Callback(Time, AliveCount),
                    {stop, normal, NewState};
                _ ->
                    {noreply, NewState}
            end;
        false ->
            {noreply, State}
    end.

handle_info({'DOWN', Ref, process, _Pid, _Info}, State) ->
    case lists:keyfind(Ref, 2, State#state.neighbours_monitors) of
        {Pos, Ref} ->
            Monitors = lists:keydelete(Ref, 2, State#state.neighbours_monitors),
            gen_server:cast(self(), {collect_cell, Pos}),
            {noreply, State#state{neighbours_monitors=Monitors}};
        _ ->
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private functions
-spec all_collected(#state{}) -> boolean().
all_collected(#state{neighbours_coordinates=X, neighbours_collected=Y}) ->
    length(X) =:= Y.

-spec collect_cell(pid(), cell:position()) -> ok.
collect_cell(CollectorPid, Position) ->
    gen_server:cast(CollectorPid, {collect_cell, Position}).

%% Tests
% -ifdef(TEST).

% start_dgol() ->
%     meck:new(dgol),
%     meck:expect(dgol, target_time, fun() -> 0 end),
%     cell_locator:start_link(),
%     gen_event:start_link({local, deb}),
%     gen_event:add_handler(deb, recorder, []).

% stop_dgol(_) ->
%     meck:unload(dgol),
%     gen_event:stop(deb),
%     cell_locator:stop().

% all_tests_test_() ->
%     {inorder, {foreach,
%      fun start_dgol/0,
%      fun stop_dgol/1,
%      [
%       fun colletor_is_able_to_collect_neighbours_content/0]}}.

% colletor_is_able_to_collect_neighbours_content() ->
%     Self = self(),
%     {ok, _} = cell:start_link({1, 1}, {3, 3}, 1),
%     {ok, _} = cell:start_link({1, 2}, {3, 3}, 0),
%     collector:start_link(
%       TimeToCollect = 0,
%       [{1, 1}, {1, 2}],
%       fun(TimeCollected, NeighboursAlive) ->
%               Self ! {collected, TimeCollected, NeighboursAlive}
%       end),
%     receive
%         {collected, T, N} ->
%             ?assertEqual(TimeToCollect, T),
%             ?assertEqual(1, N)
%     after 50 ->
%             throw(test_failed)
%     end.

% -endif.
