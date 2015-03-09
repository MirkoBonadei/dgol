-module(dgol_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%% OTP callbacks
init([]) ->
    RestartStrategy = {one_for_all, 0, 1},
    CellSup = {cell_sup, {cell_sup, start_link, []},
               permanent, infinity, supervisor, [cell_sup]},
    CellLocator = {cell_locator, {cell_locator, start_link, []},
                   permanent, 100, worker, [cell_locator]},
    Children = [CellLocator, CellSup],
    {ok, {RestartStrategy, Children}}.

