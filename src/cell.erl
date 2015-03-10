-module(cell).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([start_link/3,
         stop/1,
         get/2, 
         collected/3]).
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
              content/0]).

-record(state, { position :: position(),
                 dimensions :: dimensions(),
                 content :: content(),
                 neighbours :: neighbours(),
                 time :: time(),
                 history :: [{time(), content()}]}).

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
                                 history = [{0, InitialContent}]},
                         []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:call(Pid, stop).

-spec get(pid(), time()) -> {cell, position(), time(), content()} | future.
get(Pid, Time) ->
    gen_server:call(Pid, {get, Time}).

-spec collected(pid(), time(), non_neg_integer()) -> ok.
collected(Pid, Time, NeighboursAlive) ->
    gen_server:cast(Pid, {collected, Time, NeighboursAlive}).

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

%% TODO: check Time must be equal to State time
handle_cast({collected, Time, _NeighboursAlive}, State) ->
    NextTime = Time + 1,
    NextContent = 0,
    NextHistory = [{NextTime, NextContent} | State#state.history],
    {noreply, State#state{
                content=NextContent, 
                history=NextHistory,
                time = NextTime}}.

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

-ifdef(TEST).

cell_keeps_the_history_test() ->
    {ok, Cell} = cell:start_link({2, 2}, {5, 5}, 1),
    ?assertEqual({cell, {2, 2}, 0, 1}, cell:get(Cell, 0)),
    Time = 0,
    NeighboursAlive = 0,
    cell:collected(Cell, Time, NeighboursAlive),
    ?assertEqual({cell, {2, 2}, 0, 1}, cell:get(Cell, 0)),
    ?assertEqual({cell, {2, 2}, 1, 0}, cell:get(Cell, 1)).

-endif.
