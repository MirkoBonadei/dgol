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
                callback :: fun()}).
-define(STARTING_TIMEOUT, 10).

-spec start_link(cell:time(), cell:neighbours(), fun()) -> {ok, pid()}.
start_link(TimeToCollect, Neighbours, Callback) ->
    gen_server:start_link(?MODULE,
                          #state{time=TimeToCollect,
                                 neighbours_coordinates=Neighbours,
                                 neighbours_alive=0,
                                 neighbours_monitors=[],
                                 callback=Callback},
                          []).

init(State) ->
    timer:apply_after(10, gen_server, cast, [self(), collect_cells]),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(collect_cells, State) ->
    lists:foreach(fun(Position) ->
                          %% collector:collect_cell(self(), Position)
                          gen_server:cast(self(), {collect_cell, Position})
                  end, State#state.neighbours_coordinates),
    {noreply, State};
handle_cast({collect_cell, Position}, State) ->
    case cell_locator:get(Position) of
        {error, not_found} ->
            timer:apply_after(10, gen_server, cast, [self(), {collect_cell, Position}]),
            {noreply, State};
        Pid when is_pid(Pid) ->
            Self = self(),
            Callback = fun(Result) -> gen_server:cast(Self, Result) end,
            Ref = erlang:monitor(process, Pid),
            cell:eventually_get(Pid, State#state.time, Callback),
            NewState = State#state{neighbours_monitors=[{Position, Ref}|State#state.neighbours_monitors]},
            {noreply, NewState}
    end;
handle_cast({cell, Position, Time, Content}, State) when Time =:= State#state.time ->
    case lists:keyfind(Position, 1, State#state.neighbours_monitors) of
        {Position, Reference} ->
            NewNeighboursAlive = State#state.neighbours_alive + Content,
            NewNeighboursMonitors = lists:subtract(State#state.neighbours_monitors,
                                                      [{Position, Reference}]),
            
            erlang:demonitor(Reference),
            NewState = State#state{neighbours_monitors=NewNeighboursMonitors,
                                   neighbours_alive=NewNeighboursAlive},
            case NewNeighboursMonitors of
                [] ->
                    Callback = State#state.callback,
                    Callback(Time, NewNeighboursAlive),
                    {stop, normal, NewState};
                _ ->
                    {noreply, NewState}
            end;
        false ->
            {noreply, State}
    end.

handle_info({'DOWN', Ref, process, _Pid, _Info}, State) ->
    case lists:keyfind(Ref, 2, State#state.neighbours_monitors) of
        {Position, Ref} ->
            NewNeighboursMonitors = lists:keydelete(Ref, 2, State#state.neighbours_monitors),
            gen_server:cast(self(), {collect_cell, Position}),
            {noreply, State#state{neighbours_monitors=NewNeighboursMonitors}};
        _ ->
            %% log or crash
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-ifdef(TEST).

start_dgol() ->
    meck:new(dgol),
    meck:expect(dgol, target_time, fun() -> 0 end),
    cell_locator:start_link(),
    gen_event:start_link({local, deb}),
    gen_event:add_handler(deb, recorder, []).

stop_dgol(_) ->
    meck:unload(dgol),
    gen_event:stop(deb),
    cell_locator:stop().

all_tests_test_() ->
    {inorder, {foreach, 
     fun start_dgol/0, 
     fun stop_dgol/1, 
     [
      fun colletor_is_able_to_collect_neighbours_content/0]}}.

colletor_is_able_to_collect_neighbours_content() ->
    Self = self(),
    {ok, _} = cell:start_link({1, 1}, {3, 3}, 1),
    {ok, _} = cell:start_link({1, 2}, {3, 3}, 0),
    collector:start_link(
      TimeToCollect = 0, 
      [{1, 1}, {1, 2}],
      fun(TimeCollected, NeighboursAlive) ->
              Self ! {collected, TimeCollected, NeighboursAlive}
      end),
    receive
        {collected, T, N} ->
            ?assertEqual(TimeToCollect, T),
            ?assertEqual(1, N)
    after 50 ->
            throw(test_failed)
    end.

-endif.
