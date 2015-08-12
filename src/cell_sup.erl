-module(cell_sup).
-behaviour(supervisor).

-export([start_link/0, 
         start_cell/3]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_cell(cell:position(), cell:dimensions(), cell:content()) -> 
                        supervisor:startchild_ret().
start_cell(Position, Dimensions, InitialContent) ->
    {ok, _} = supervisor:start_child(?MODULE,
                                     child_spec(Position,
                                                Dimensions,
                                                InitialContent)).

%%% OTP supervisor callback

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 100000,
    MaxSecondsBetweenRestarts = 1,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, []}}.

%%% private functions

-spec child_spec(cell:position(), cell:dimensions(), cell:content()) -> 
                        supervisor:child_spec().
child_spec(Position, Dimensions, InitialContent) ->
    {{cell, Position}, {cell, start_link, [Position, Dimensions, InitialContent]},
     transient, infinity, worker, [cell]}.
