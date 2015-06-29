-module(dgol_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

-ifdef(TEST).
add_handlers() -> ok.
-else.
add_handlers() -> ok = gen_event:add_handler(deb, ui, []),
                  ok = gen_event:add_handler(deb, gui, []).
-endif.

start_link() ->
    Res = {ok, _Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    add_handlers(),
    Res.

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

