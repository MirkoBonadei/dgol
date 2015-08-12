%% @doc It reppresents a Game Of Life cell. Each cell is a process and
%% it is responsable to keep its current state (and its history).
-module(cell).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").
-include_lib("eunit_macros.hrl").

-export([start_link/3,
         stop/1,
         get/2,
         eventually_get/3,
         collected/3,
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
-type future() :: {time(), fun()}.

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
                collecting :: boolean(),
                history :: sets:set(time()),
                future :: [future()]}).

%% API functions

%% @doc Starts a cell process.
%% The cell will be registered on the local instance of the cell_locator
%% and it will try to understand to target_time of the universe to start
%% chasing it. An event of type cell_born will be emitted.
-spec start_link(Dim :: dimensions(), Pos :: position(), Content :: content()) ->
                        {ok, pid()} |
                        ignore |
                        {error, already_started} |
                        {error, term()}.
start_link({Xd, Yd} = Dim, {Xp, Yp} = Pos, Content)
  when Xp < Xd,
       0 =< Xp,
       Yp < Yd,
       0 =< Yp ->
    gen_server:start_link(?MODULE,
                          #state{position = Pos,
                                 dimensions = Dim,
                                 content = Content,
                                 neighbours = compute_neighbours(Pos, Dim),
                                 time = 0,
                                 target_time=0,
                                 collecting=false,
                                 history = add_to_history(0, Content, sets:new()),
                                 future = []},
                         []).
%% @doc Stops the cell.
-spec stop(Pid :: pid()) -> ok.
stop(Pid) ->
    gen_server:call(Pid, stop).

%% @doc Gets the content of the cell at a specified time with a synchronous call.
-spec get(Pid :: pid(), T :: time()) ->
                 {cell, position(), time(), content()} |
                 future.
get(Pid, Time) ->
    gen_server:call(Pid, {get, Time}).

%% @doc Gets the content of the cell at a specified time with an asynchronous call.
%% The third parameter is a callback which will be invoked when the content will
%% be available.
-spec eventually_get(Pid :: pid(), T :: time(), Callback :: fun()) -> ok.
eventually_get(Pid, T, Callback) ->
    gen_server:cast(Pid, {eventually_get, T, Callback}).

%% @doc Notified the cell that a collection process is completed passing
%% the number of neighbours alive at the given time.
%% This is invoked by the collector process when it knows the content of
%% all the neighbours of the cell.
-spec collected(Pid :: pid(), T :: time(), Counter :: non_neg_integer()) -> ok.
collected(Pid, T, Counter) ->
    gen_server:cast(Pid, {collected, T, Counter}).

%% @doc Ask the cell to evolve at the specified time.
-spec evolve_at(Pid :: pid(), T :: time()) -> ok.
evolve_at(Pid, T) ->
    gen_server:cast(Pid, {evolve_at, T}).

%% gen_server callbacks

init(S) ->
    cell_locator:put(S#state.position, self()),
    gen_event:notify(deb, {cell_born,
                           S#state.position,
                           S#state.content}),
    cell:evolve_at(self(), target_time),
    {ok, S}.

handle_call({get, T}, _From, S) when T > S#state.time ->
    {reply, future, S};
handle_call({get, T}, _From, S) ->
    Content = content_from_history(T, S#state.history),
    {reply, {cell, S#state.position, T, Content}, S};
handle_call(stop, _From, S) ->
    {stop, normal, ok, S}.

handle_cast({collected, T, NeighboursAlive}, S) when T =:= S#state.time ->
    NextTime = T + 1,
    NextContent = evolve(S#state.content, NeighboursAlive),
    NextHistory = add_to_history(NextTime, NextContent, S#state.history),
    {KnownFutures, UnknownFutures} = split_known_futures(NextTime, S#state.future),
    reply_known_futures({cell, S#state.position, NextTime, NextContent}, KnownFutures),
    gen_event:notify(deb, {cell_evolved,
                           S#state.position,
                           NextContent,
                           NextTime}),
    NextState = S#state{content=NextContent,
                        history=NextHistory,
                        future=UnknownFutures,
                        time=NextTime},
    case S#state.target_time > NextTime of
        true ->
            TimeToCollect = NextTime,
            NeighboursPositions = S#state.neighbours,
            collect(TimeToCollect, NeighboursPositions),
            {noreply, NextState#state{collecting=true}};
        _ ->
            {noreply, NextState#state{collecting=false}}
    end;
handle_cast({collected, _T, _NeighboursAlive}, S) ->
    {noreply, S};
handle_cast({eventually_get, T, Callback}, S) when T > S#state.time ->
    NextState = S#state{future=[{T, Callback}|S#state.future]},
    {noreply, NextState};
handle_cast({eventually_get, T, Callback}, S) ->
    Content = content_from_history(T, S#state.history),
    Callback({cell, S#state.position, T, Content}),
    {noreply, S};
handle_cast({evolve_at, target_time}, S) ->
    cell:evolve_at(self(), dgol:target_time()),
    {noreply, S};
handle_cast({evolve_at, T}, S) when T =< S#state.time ->
    gen_event:notify(deb, {already_evolved,
                           S#state.position,
                           S#state.content,
                           T}),
    {noreply, S};
handle_cast({evolve_at, T}, S) when T =< S#state.target_time ->
    gen_event:notify(deb, {already_evolving,
                           S#state.position,
                           S#state.target_time}),
    {noreply, S};
handle_cast({evolve_at, T}, S) when S#state.collecting ->
    {noreply, S#state{target_time=T}};
handle_cast({evolve_at, T}, S) ->
    TimeToCollect = S#state.time,
    NeighboursPositions = S#state.neighbours,
    collect(TimeToCollect, NeighboursPositions),
    {noreply, S#state{target_time=T, collecting=true}}.

handle_info(_Request, S) ->
    {noreply, S}.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

terminate(Reason, S) ->
    gen_event:notify(deb, {cell_died, S#state.position, Reason}),
    ok.

%% private functions

%% @doc Returns a list with the neighbours's coordinate of a
%% cell.
%% @private
-spec compute_neighbours(Pos :: position(), Dim :: dimensions()) -> [position()].
compute_neighbours({Xp, Yp}, {Xd, Yd}) ->
    [{Xa rem Xd, Ya rem Yd} ||
        Xa <- lists:seq(Xp - 1 + Xd, Xp + 1 + Xd),
        Ya <- lists:seq(Yp - 1 + Yd, Yp + 1 + Yd),
        {Xa, Ya} /= {Xp + Xd, Yp + Yd}].

%% @doc The heart of the game of life.
%% If a cell has 2 neighbours alive and it is alive, then
%% it stays alive. If a cell has 3 neighbours alive then it
%% stays alive or it became alive. In any other cases the cell
%% dies.
%% @private
-spec evolve(Content :: content(), NeighboursAlive :: non_neg_integer()) ->
                    NextContent :: content().
evolve(1, 2) ->
    1;
evolve(_, 3) ->
    1;
evolve(_, _) ->
    0.

%% @doc Given the list of the future actions to be done and a time,
%% it returns a tuple with the action that can be done now (because
%% the actual time is the same as the action schedule time) as first
%% element and the action to be done in the future as second element.
%% @private
-spec split_known_futures(T :: time(), [future()]) ->
                                 {[future()], [future()]}.
split_known_futures(T, Futures) ->
    KnownFutures = lists:filter(fun({FutureTime, _Callback}) ->
                                        FutureTime =:= T
                                end, Futures),
    UnknownFutures = lists:subtract(Futures, KnownFutures),
    {KnownFutures, UnknownFutures}.

%% @doc Foreach futures it invokes the future's callback with
%% the message as parameter.
%% @private
-spec reply_known_futures(Message :: term(), Futures :: [future()]) ->
                                 ok.
reply_known_futures(Message, Futures) ->
    lists:foreach(fun({_Time, Callback}) ->
                          Callback(Message)
                  end, Futures),
    ok.

%% @doc Starts a collector process to collect the content of the
%% neighbours at a specified time. The collector response is
%% asynchronous and it will come later in the future as a notification
%% on the public API of the cell (see function collected/3).
%% @private
-spec collect(T :: time(), NeighboursPos :: [position(), ...]) -> ok.
collect(T, NeighboursPos) ->
    Self = self(),
    Callback = fun(TimeCollected, NeighboursAlive) when TimeCollected =:= T ->
                       cell:collected(Self, TimeCollected, NeighboursAlive)
               end,
    {ok, _Pid} = collector:start_link(T,
                                      NeighboursPos,
                                      Callback),
    ok.

%% @doc Adds a time to the set of the times in which the cell was alive
%% in the past.
%% The choice of using a set is purely to save memory and make the game last
%% longer.
%% @private
-spec add_to_history(T :: time(), C :: content(), Set :: sets:set(time())) ->
                            sets:set(time()).
add_to_history(_T, 0, Set) ->
    Set;
add_to_history(T, 1, Set) ->
    sets:add_element(T, Set).

%% @doc Fetches the content of the cell from the history set for a
%% specified time.
%% @private
-spec content_from_history(T :: time(), Set :: sets:set(time())) ->
                                  content().
content_from_history(T, Set) ->
    case sets:is_element(T, Set) of
        true ->
            1;
        _ ->
            0
    end.

%% tests
-ifdef(TEST).

all_tests_test_() ->
    {inorder, {foreach,
               fun setup/0,
               fun teardown/1,
               [
                fun cell_keeps_the_history/0,
                fun cell_cannot_predict_the_future/0,
                fun cell_refuses_collected_in_the_past_or_in_the_future/0,
                fun cell_eventually_get_in_the_past/0,
                fun cell_eventually_get_in_the_future/0,
                fun cell_eventually_get_supports_multiple_requests/0,
                fun cell_refuses_to_evolve_to_time_already_in_target/0
               ]}}.

setup() ->
    meck:new(dgol),
    meck:new(cell_locator),
    meck:new(collector),
    meck:expect(dgol, target_time, fun() -> 0 end),
    meck:expect(cell_locator, put, fun(_Pos, _Pid) -> ok end),
    meck:expect(collector, start_link, [{3, {ok, pid}}]),
    gen_event:start_link({local, deb}),
    gen_event:add_handler(deb, recorder, []).

teardown(_) ->
    meck:unload(dgol),
    meck:unload(cell_locator),
    meck:unload(collector),
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
    ?assertReceive({cell,
                    {2, 2},
                    _ExpectedTime = 0,
                    _ExpectedContent = 1},
                   50).

cell_eventually_get_in_the_future() ->
    Self = self(),
    {ok, Cell} = cell:start_link({2, 2}, {5, 5}, 1),
    cell:eventually_get(Cell, 1, fun(Result) -> Self ! Result end),
    cell:collected(Cell, 0, 3),
    ?assertReceive({cell,
                    {2, 2},
                    _ExpectedTime = 1,
                    _ExpectedContent = 1},
                   50).

cell_eventually_get_supports_multiple_requests() ->
    Self = self(),
    {ok, Cell} = cell:start_link({2, 2}, {5, 5}, 1),
    cell:eventually_get(Cell, 1, fun(Result) -> Self ! Result end),
    cell:eventually_get(Cell, 1, fun(Result) -> Self ! Result end),
    cell:collected(Cell, 0, 3),
    ?assertReceive({cell,
                    {2, 2},
                    _ExpectedTime = 1,
                    _ExpectedContent = 1},
                   50),
    ?assertReceive({cell,
                    {2, 2},
                    _ExpectedTime = 1,
                    _ExpectedContent = 1},
                   50).

cell_refuses_to_evolve_to_time_already_in_target() ->
    {ok, Cell} = cell:start_link({2, 2}, {5, 5}, 1),
    ok = cell:evolve_at(Cell, 2),
    cell:evolve_at(Cell, 1),
    timer:sleep(200), %% TODO: add wait for event...
    ?assert(recorder:is_recorded({already_evolving, {2, 2}, 2})).

-endif.
