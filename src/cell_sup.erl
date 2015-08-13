-module(cell_sup).
-behaviour(supervisor).

-export([start_link/1,
         start_cell/2]).
-export([init/1]).

-spec start_link(cell:dimensions()) -> supervisor:startlink_ret().
start_link(Dimensions) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Dimensions]).

-spec start_cell(cell:position(), cell:content()) -> supervisor:startchild_ret().
start_cell(Position, InitialContent) ->
    supervisor:start_child(?MODULE, [Position, InitialContent]).

%%% OTP supervisor callback

init([{Xd, Yd}]) ->
    RestartStrategy = {simple_one_for_one, Xd * Yd, 1},
    CellTemplate = {cell, {cell, start_link, [{Xd, Yd}]},
                    transient, infinity, worker, [cell]},
    {ok, {RestartStrategy, [CellTemplate]}}.
