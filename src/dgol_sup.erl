-module(dgol_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

-ifndef(TEST).
-define(TEST, false).
-endif.

start_link() ->
    {ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    case ?TEST of  
        false -> ok = gen_event:add_handler(deb, ui, []),
                 ok = gen_event:add_handler(deb, clock, []),
                 {ok, Pid};
        true ->
            {ok, Pid}
    end.

%%% OTP callbacks
init([]) ->
    RestartStrategy = {one_for_all, 0, 1},
    CellSup = {cell_sup, {cell_sup, start_link, []},
               permanent, infinity, supervisor, [cell_sup]},
    CellLocator = {cell_locator, {cell_locator, start_link, []},
                   permanent, 100, worker, [cell_locator]},
    DomainEventBus = {deb, {gen_event, start_link, [{local, deb}]},
                      permanent, 1000, worker, [dynamic]},

    Children = [DomainEventBus, CellLocator, CellSup],
    {ok, {RestartStrategy, Children}}.

