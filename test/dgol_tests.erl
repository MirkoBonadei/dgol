-module(dgol_tests).
-include_lib("eunit/include/eunit.hrl").

cell_can_collect_number_of_neighbours_alive_for_a_given_time_test() ->
    {ok, _LocatorPid} = cell_locator:start_link(),
    {ok, CellSupPid} = cell_sup:start_link(),
    Self = self(),
    BoardDimensions = {5, 5},
    {ok, Cell} = cell:start_link({2, 2}, BoardDimensions, 1),
    Neighbours = [{X, Y} || X <- lists:seq(1, 3),
                            Y <- lists:seq(1, 3),
                            {X, Y} =/= {2, 2}],
    lists:foreach(fun(NeighbourPosition) ->
                          {ok, _CellPid} = cell_sup:start_cell(NeighbourPosition, BoardDimensions, 0)
                  end, Neighbours),
    cell:evolve(Cell),
    cell:eventually_get(Cell, 1, fun(Result) -> Self ! Result end),
    assertReceive({cell, {2, 2}, _ExpectedTime = 1, _ExpectedContent = 0}, 50),
    cell_locator:stop(),
    exit(CellSupPid, normal).

assertReceive(ExpectedMessage, Timeout) ->
    receive
        ReceivedMessage ->
            ?assertMatch(ExpectedMessage, ReceivedMessage)
    after Timeout ->
            ?assertMatch(ExpectedMessage, false)
    end.

