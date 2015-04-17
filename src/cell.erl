-module(cell).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([start_link/3,
         stop/1,
         get/2, 
         eventually_get/3,
         collected/3,
         evolve/1,
         evolve_at/2]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type position() :: {non_neg_integer(), non_neg_integer()}.
-type dimensions() :: {pos_integer(), pos_integer()}.
-type content() :: 0 | 1.
-type neighbours() :: [position()].
-type time() :: non_neg_integer().

-export_type([position/0,
              dimensions/0,
              content/0,
              neighbours/0,
              time/0]).

-record(state, {position :: position(),
                dimensions :: dimensions(),
                content :: content(),
                neighbours :: neighbours(),
                time :: time(),
                target_time :: time(),
                history :: [{time(), content()}],
                future :: [{time(), fun()}]}).

%%% TODO: solve this internal conflict of trying to spec the return values of 
%%% OTP.
-spec start_link(position(), dimensions(), content()) -> {ok, pid()}.
start_link({X, Y} = Pos, {Xdim, Ydim} = Dim, InitialContent)
  when X < Xdim,
       0 =< X,
       Y < Ydim,
       0 =< Y ->
    gen_server:start_link(?MODULE,
                          #state{position = Pos,
                                 dimensions = Dim,
                                 content = InitialContent,
                                 neighbours = compute_neighbours(Pos, Dim),
                                 time = 0,
                                 target_time=0,
                                 history = [{0, InitialContent}],
                                 future = []},
                         []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:call(Pid, stop).

-spec get(pid(), time()) -> {cell, position(), time(), content()} | future.
get(Pid, Time) ->
    gen_server:call(Pid, {get, Time}).

-spec eventually_get(pid(), time(), fun()) -> ok.
eventually_get(Pid, Time, Callback) ->
    gen_server:cast(Pid, {eventually_get, Time, Callback}).

-spec collected(pid(), time(), non_neg_integer()) -> ok.
collected(Pid, Time, NeighboursAlive) ->
    gen_server:cast(Pid, {collected, Time, NeighboursAlive}).

-spec evolve(pid()) -> ok.
evolve(Pid) ->
    gen_server:cast(Pid, evolve).

-spec evolve_at(pid(), time()) -> ok.
evolve_at(Pid, Time) ->
    gen_server:cast(Pid, {evolve_at, Time}).

%%% OTP gen_server callbacks

init(State) ->
    gen_event:notify(deb, {cell_born, 
                           State#state.position, 
                           State#state.content}),
    {ok, State}.

handle_call({get, Time}, _From, State) ->
    case lists:keyfind(Time, 1, State#state.history) of
        {Time, Content} ->
            {reply, {cell, State#state.position, Time, Content}, State};
        false ->
            {reply, future, State}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({collected, Time, NeighboursAlive}, State) when Time =:= State#state.time ->
    NextTime = Time + 1,
    NextContent = evolve(State#state.content, NeighboursAlive),
    NextHistory = [{NextTime, NextContent} | State#state.history],
    {KnownFutures, UnknownFutures} = split_known_futures(NextTime, State#state.future),
    reply_known_futures({cell, State#state.position, NextTime, NextContent}, KnownFutures),
    gen_event:notify(deb, {cell_evolved, 
                           State#state.position,
                           NextContent,
                           NextTime}),
    TargetTime = case State#state.target_time > NextTime of
                     true ->
                         TimeToCollect = NextTime,
                         NeighboursPositions = State#state.neighbours,
                         collect(TimeToCollect, NeighboursPositions),
                         State#state.target_time;
                     _ -> 
                         State#state.time
                 end,
    {noreply, State#state{
                content=NextContent, 
                history=NextHistory,
                future=UnknownFutures,
                time = NextTime,
                target_time=TargetTime}};
handle_cast({collected, _Time, _NeighboursAlive}, State) ->
    {noreply, State};
handle_cast({eventually_get, Time, Callback}, State) ->
    case lists:keyfind(Time, 1, State#state.history) of
        {Time, Content} ->
            Callback({cell, State#state.position, Time, Content}),
            {noreply, State};
        false ->
            NextState = State#state{future=[{Time, Callback}|State#state.future]},
            {noreply, NextState}
    end;
handle_cast(evolve, State) ->
    TimeToCollect = State#state.time,
    NeighboursPositions = State#state.neighbours,
    collect(TimeToCollect, NeighboursPositions),
    {noreply, State};
handle_cast({evolve_at, Time}, State) when Time =< State#state.time ->
    gen_event:notify(deb, {already_evolved, 
                           State#state.position, 
                           State#state.content,
                           Time}),
    {noreply, State};
handle_cast({evolve_at, Time}, State) ->
    TimeToCollect = State#state.time,
    NeighboursPositions = State#state.neighbours,
    collect(TimeToCollect, NeighboursPositions),
    {noreply, State#state{target_time=Time}}.

handle_info(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, State) ->
    gen_event:notify(deb, {cell_died, State#state.position, Reason}),
    ok.
    
%%% private functions

-spec compute_neighbours(position(), dimensions()) -> [position()].
compute_neighbours({X, Y}, {Xdim, Ydim}) ->
    [ {Xa rem Xdim, Ya rem Ydim} ||
        Xa <- lists:seq(X - 1 + Xdim, X + 1 + Xdim),
        Ya <- lists:seq(Y - 1 + Ydim, Y + 1 + Ydim),
        {Xa, Ya} /= {X + Xdim, Y + Ydim}].

-spec evolve(CurrentContent :: content(), NeighboursAlive :: non_neg_integer()) -> 
                    NextContent :: content().
evolve(1, 2) ->
    1;
evolve(_, 3) ->
    1;
evolve(_, _) ->
    0.

split_known_futures(Time, Futures) ->
    KnownFutures = lists:filter(fun({FutureTime, _Callback}) ->  FutureTime =:= Time
                                 end, Futures),
    UnknownFutures = lists:subtract(Futures, KnownFutures),
    {KnownFutures, UnknownFutures}.

reply_known_futures(Message, Futures) ->
    lists:foreach(fun({_Time, Callback}) -> 
                          Callback(Message)
                  end, Futures).

-spec collect(TimeToCollect :: time(), NeighboursPositions :: [position(), ...]) -> ok.
collect(TimeToCollect, NeighboursPositions) ->
    Self = self(),
    {ok, _CollectorPid} = collector:start_link(TimeToCollect,
                                               NeighboursPositions,
                                               fun(TimeCollected, NeighboursAlive) when TimeCollected =:= TimeToCollect ->
                                                       cell:collected(Self, TimeCollected, NeighboursAlive)
                                               end),
    ok.

-ifdef(TEST).

all_tests_test_() ->
    {inorder, {foreach, 
               fun setup/0,
               fun teardown/1,
               [fun cell_keeps_the_history/0,
                fun cell_cannot_predict_the_future/0,
                fun cell_refuses_collected_in_the_past_or_in_the_future/0,
                fun cell_eventually_get_in_the_past/0,
                fun cell_eventually_get_in_the_future/0,
                fun cell_eventually_get_supports_multiple_requests/0]}}.

setup() ->
    gen_event:start_link({local, deb}).

teardown(_) ->
    gen_event:stop(deb).

cell_keeps_the_history() ->
    {ok, Cell} = cell:start_link({2, 2}, {5, 5}, 1),
    ?assertEqual({cell, {2, 2}, 0, 1}, cell:get(Cell, 0)),
    Time = 0,
    NeighboursAlive = 0,
    cell:collected(Cell, Time, NeighboursAlive),
    ?assertEqual({cell, {2, 2}, 0, 1}, cell:get(Cell, 0)),
    ?assertEqual({cell, {2, 2}, 1, 0}, cell:get(Cell, 1)).

cell_cannot_predict_the_future() ->
    {ok, Cell} = cell:start_link({2, 2}, {5, 5}, 1),
    ?assertEqual(future, cell:get(Cell, 1)).

cell_refuses_collected_in_the_past_or_in_the_future() ->
    {ok, Cell} = cell:start_link({2, 2}, {5, 5}, 1),
    cell:collected(Cell, 0, 3),
    ?assertEqual({cell, {2, 2}, 1, 1}, cell:get(Cell, 1)),
    cell:collected(Cell, 0, 0),
    ?assertEqual({cell, {2, 2}, 1, 1}, cell:get(Cell, 1)),
    cell:collected(Cell, 5, 2),
    ?assertEqual(future, cell:get(Cell, 6)).

cell_eventually_get_in_the_past() ->
    Self = self(),
    {ok, Cell} = cell:start_link({2, 2}, {5, 5}, 1),
    cell:collected(Cell, 0, 3),
    cell:collected(Cell, 1, 2),
    cell:eventually_get(Cell, 0, fun(Result) -> Self ! Result end),
    assertReceive({cell, {2, 2}, _ExpectedTime = 0, _ExpectedContent = 1}, 50).

cell_eventually_get_in_the_future() ->
    Self = self(),
    {ok, Cell} = cell:start_link({2, 2}, {5, 5}, 1),
    cell:eventually_get(Cell, 1, fun(Result) -> Self ! Result end),
    cell:collected(Cell, 0, 3),
    assertReceive({cell, {2, 2}, _ExpectedTime = 1, _ExpectedContent = 1}, 50).

cell_eventually_get_supports_multiple_requests() ->
    Self = self(),
    {ok, Cell} = cell:start_link({2, 2}, {5, 5}, 1),
    cell:eventually_get(Cell, 1, fun(Result) -> Self ! Result end),
    cell:eventually_get(Cell, 1, fun(Result) -> Self ! Result end),
    cell:collected(Cell, 0, 3),
    assertReceive({cell, {2, 2}, _ExpectedTime = 1, _ExpectedContent = 1}, 50),
    assertReceive({cell, {2, 2}, _ExpectedTime = 1, _ExpectedContent = 1}, 50).

assertReceive(ExpectedMessage, Timeout) ->
    receive
        ReceivedMessage ->
            ?assertMatch(ExpectedMessage, ReceivedMessage)
    after Timeout ->
            ?assertMatch(ExpectedMessage, false)
    end.    

-endif.
