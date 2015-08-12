-module(cell_sup).
-behaviour(supervisor).

-export([start_link/2,
         start_cell/2]).
-export([init/1]).

-spec start_link(cell:dimensions()) -> supervisor:startlink_ret().
start_link(Dimensions) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Dimensions]).

-spec start_cell(cell:position(), cell:content()) -> supervisor:startchild_ret().
start_cell(Position, InitialContent) ->
    Cell = {{cell, Position}, {cell, start_link, [Position, InitialContent]},
            transient, infinity, worker, [cell]},
    {ok, _} = supervisor:start_child(?MODULE, Cell).

%%% OTP supervisor callback

init([{XDim, YDim}]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = XDim * YDim,
    MaxSecondsBetweenRestarts = 1,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, []}}.
