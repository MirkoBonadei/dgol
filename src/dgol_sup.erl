-module(dgol_sup).

-behaviour(supervisor).

-export([start_link/0,
         start_dgol_session/3]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_dgol_session(pos_integer(), pos_integer(), [cell:position(), ...]) -> 
                                supervisor:startchild_ret().
start_dgol_session(Xdim, Ydim, InitialCells) ->
    case whereis(dgol) of
        undefined ->
            supervisor:start_child(?SERVER, 
                                   {dgol, {dgol, start_link, [Xdim, Ydim, InitialCells]},
                                           permanent, 2000, worker, [dgol]});
        _ ->
            {error, already_started}
    end.
    


%%% OTP callbacks

init([]) ->
    RestartStrategy = {one_for_all, 0, 1},
    CellSup = {cell_sup, {cell_sup, start_link, []},
               permanent, infinity, supervisor, [cell_sup]},
    Children = [CellSup],
    {ok, {RestartStrategy, Children}}.

