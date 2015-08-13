-module(universe_sup).
-behaviour(supervisor).
-export([start_link/4]).
-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link(pos_integer(), pos_integer(), [cell:position(), ...], [{module(), [term()]}]) ->
                        supervisor:startlink_ret().
start_link(XDim, YDim, InitialCells, EventHandlers) ->
    {ok, _Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, [XDim, YDim, InitialCells, EventHandlers]).

%%% OTP callbacks
% TODO: remove EventHandlers
init([XDim, YDim, InitialCells, _EventHandlers]) ->
    RestartStrategy = {one_for_all, 0, 1},
    CellLocator = {cell_locator, {cell_locator, start_link, []},
                   permanent, 100, worker, [cell_locator]},
    CellSup = {cell_sup, {cell_sup, start_link, [{XDim, YDim}]},
               permanent, infinity, supervisor, [cell_sup]},
    Universe = {universe, {universe, start_link, [{XDim, YDim}, InitialCells]},
                permanent, infinity, worker, [universe]},
    {ok, {RestartStrategy, [CellLocator, CellSup, Universe]}}.
