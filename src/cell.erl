-module(cell).
-behaviour(gen_server).

-export([start_link/3,
         stop/1]).
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

-record(state, { position :: position(),
                 dimensions :: dimensions(),
                 content :: content(),
                 neighbours :: neighbours()}).

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
                                 neighbours = compute_neighbours(Pos, Dim)},
                         []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:call(Pid, stop).

%%% OTP gen_server callbacks

init(State) ->
    {ok, State}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Request, State) ->
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
