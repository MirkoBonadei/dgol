-module(dgol_sup).
-behaviour(supervisor).
-export([start_link/0,
         start_universe/4]).
-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    {ok, _Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_universe(pos_integer(), pos_integer(), [cell:position(), ...], [{module(), [term()]}]) ->
                        supervisor:startchild_ret().
start_universe(XDim, YDim, InitialCells, EventHandlers) ->
    UniverseSup = {universe_sup, {universe_sup, start_link, [XDim, YDim, InitialCells, EventHandlers]},
                   transient, infinity, supervisor, [universe_sup]},
    supervisor:start_child(?MODULE, UniverseSup).

%%% OTP callbacks
init([]) ->
    RestartStrategy = {one_for_rest, 0, 1},
    DomainEventBus = {deb, {gen_event, start_link, [{local, deb}]},
                      permanent, 1000, worker, [dynamic]},
    {ok, {RestartStrategy, [DomainEventBus]}}.
