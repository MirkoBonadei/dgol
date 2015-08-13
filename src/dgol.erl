-module(dgol).

-export([start_universe/3,
         start_universe/4,
         start_universe_and_wait/4]).


-spec start_universe(pos_integer(), pos_integer(), [cell:position(), ...]) ->
                           supervisor:startchild_ret().
start_universe(Xd, Yd, InitialCells) ->
    dgol:start_universe(Xd, Yd, InitialCells, []).

-spec start_universe(pos_integer(), pos_integer(), [cell:position(), ...], [{module(), [term()]}]) ->
                           supervisor:startchild_ret().
start_universe(Xd, Yd, InitialCells, EventHandlers) ->
    dgol_sup:start_universe(Xd, Yd, InitialCells, EventHandlers).

-spec start_universe_and_wait(pos_integer(), pos_integer(), [cell:position(), ...], timeout()) ->
                           supervisor:startchild_ret() | {error, timeout}.
start_universe_and_wait(Xd, Yd, InitialCells, Timeout) ->
    case dgol:start_universe(Xd, Yd, InitialCells) of
        Error = {error, _} ->
            Error;
        Success ->
            Positions = [{X, Y} || X <- lists:seq(0, Xd - 1),
                                   Y <- lists:seq(0, Yd - 1)],
            case cell_locator:wait_for_all(Positions, Timeout) of
                timeout ->
                    {error, timeout};
                ok ->
                    Success
            end
    end.
