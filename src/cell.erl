-module(cell).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([start_link/3,
         stop/1,
         get/2, 
         eventually_get/3,
         collected/3,
         evolve/1]).
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

%%% OTP gen_server callbacks

init(State) ->
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

    {noreply, State#state{
                content=NextContent, 
                history=NextHistory,
                future=UnknownFutures,
                time = NextTime}};
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
    Self = self(),
    {ok, _CollectorPid} = collector:start_link(TimeToCollect,
                                               State#state.neighbours,
                                               fun(TimeCollected, NeighboursAlive) when TimeCollected =:= TimeToCollect ->
                                                       cell:collected(Self, TimeCollected, NeighboursAlive)
                                               end),
    {noreply, State}.


handle_info(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
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

-ifdef(TEST).

cell_keeps_the_history_test() ->
    {ok, Cell} = cell:start_link({2, 2}, {5, 5}, 1),
    ?assertEqual({cell, {2, 2}, 0, 1}, cell:get(Cell, 0)),
    Time = 0,
    NeighboursAlive = 0,
    cell:collected(Cell, Time, NeighboursAlive),
    ?assertEqual({cell, {2, 2}, 0, 1}, cell:get(Cell, 0)),
    ?assertEqual({cell, {2, 2}, 1, 0}, cell:get(Cell, 1)).

cell_cannot_predict_the_future_test() ->
    {ok, Cell} = cell:start_link({2, 2}, {5, 5}, 1),
    ?assertEqual(future, cell:get(Cell, 1)).

cell_refuses_collected_in_the_past_or_in_the_future_test() ->
    {ok, Cell} = cell:start_link({2, 2}, {5, 5}, 1),
    cell:collected(Cell, 0, 3),
    ?assertEqual({cell, {2, 2}, 1, 1}, cell:get(Cell, 1)),
    cell:collected(Cell, 0, 0),
    ?assertEqual({cell, {2, 2}, 1, 1}, cell:get(Cell, 1)),
    cell:collected(Cell, 5, 2),
    ?assertEqual(future, cell:get(Cell, 6)).

cell_eventually_get_in_the_past_test() ->
    Self = self(),
    {ok, Cell} = cell:start_link({2, 2}, {5, 5}, 1),
    cell:collected(Cell, 0, 3),
    cell:collected(Cell, 1, 2),
    cell:eventually_get(Cell, 0, fun(Result) -> Self ! Result end),
    assertReceive({cell, {2, 2}, _ExpectedTime = 0, _ExpectedContent = 1}, 50).

cell_eventually_get_in_the_future_test() ->
    Self = self(),
    {ok, Cell} = cell:start_link({2, 2}, {5, 5}, 1),
    cell:eventually_get(Cell, 1, fun(Result) -> Self ! Result end),
    cell:collected(Cell, 0, 3),
    assertReceive({cell, {2, 2}, _ExpectedTime = 1, _ExpectedContent = 1}, 50).

cell_eventually_get_supports_multiple_requests_test() ->
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
