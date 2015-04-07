-module(dgol_tests).
-include_lib("eunit/include/eunit.hrl").

-export([start_dgol/0
        ,stop_dgol/1]).

all_tests_test_() ->
    {inorder, {foreach, 
               fun start_dgol/0, 
               fun stop_dgol/1, 
               [
                fun cell_can_collect_number_of_neighbours_alive_for_a_given_time/0,
                fun cells_can_evolve_together/0]}}.

start_dgol() ->
    application:start(dgol).

stop_dgol(_) ->
    error_logger:tty(false),
    application:stop(dgol),
    error_logger:tty(true).

cell_can_collect_number_of_neighbours_alive_for_a_given_time() ->
    Self = self(),
    CellPosition = {2, 2},
    {ok, _} = dgol:start_session_and_wait(5, 5, [CellPosition], 50),
    
    CellPid = cell_locator:get(CellPosition),

    cell:evolve(CellPid),
    timer:sleep(500),
    cell:eventually_get(CellPid, 1, fun(Result) -> Self ! Result end),
    assertReceive({cell, {2, 2}, _ExpectedTime = 1, _ExpectedContent = 0}, 50).

cells_can_evolve_together() ->
    StepOneLivingCells = [{2, 1}, {2, 2}, {2, 3}],
    StepTwoLivingCells = [{1, 2}, {2, 2}, {3, 2}],
    {ok, _} = dgol:start_session_and_wait(5, 5, StepOneLivingCells, 50),

    ?assertMatch(
       [{_, _, _, 1}, {_, _, _, 1}, {_, _, _, 1}], 
       [cell:get(cell_locator:get(Position), 0) || Position <- StepOneLivingCells]),
    dgol:evolve(),
    timer:sleep(500),
    ?assertMatch(
       [{_, _, _, 1}, {_, _, _, 1}, {_, _, _, 1}], 
       [cell:get(cell_locator:get(Position), 1) || Position <- StepTwoLivingCells]).

assertReceive(ExpectedMessage, Timeout) ->
    receive
        ReceivedMessage ->
            ?assertMatch(ExpectedMessage, ReceivedMessage)
    after Timeout ->
            ?assertMatch(ExpectedMessage, false)
    end.

