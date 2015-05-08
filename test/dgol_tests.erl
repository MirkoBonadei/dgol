-module(dgol_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("eunit_macros.hrl").

-export([start_dgol/0
        ,stop_dgol/1]).

all_tests_test_() ->
    {inorder, {foreach, 
               fun start_dgol/0, 
               fun stop_dgol/1, 
               [
                fun cell_can_evolve_at_next_time/0,
                fun cells_can_evolve_together/0,
                fun cell_does_not_evolve_when_already_evolved/0,
                fun cell_can_evolve_at_future_time/0,
                fun events_about_cells_are_emitted_correctly/0,
                fun cell_register_itself_after_the_rebirth/0,
                fun cell_can_recover_after_death/0
               ]}}.

start_dgol() ->
    application:start(dgol),
    gen_event:add_handler(deb, recorder, []).

stop_dgol(_) ->
    error_logger:tty(false),
    application:stop(dgol),
    error_logger:tty(true).

cell_does_not_evolve_when_already_evolved() ->
    CellPosition = {2, 2},
    {ok, _} = dgol:start_session_and_wait(5, 5, [CellPosition], 50),
    
    CellPid = cell_locator:get(CellPosition),

    cell:evolve_at(CellPid, 0),
    timer:sleep(500),
    ?assert(recorder:is_recorded({already_evolved, {2, 2}, 1, 0})).

cell_can_evolve_at_next_time() ->
    Self = self(),
    CellPosition = {2, 2},
    {ok, _} = dgol:start_session_and_wait(5, 5, [CellPosition], 50),
    
    CellPid = cell_locator:get(CellPosition),

    cell:evolve_at(CellPid, 1),
    timer:sleep(500),
    cell:eventually_get(CellPid, 1, fun(Result) -> Self ! Result end),
    ?assertReceive({cell, {2, 2}, _ExpectedTime = 1, _ExpectedContent = 0}, 50).

cell_can_evolve_at_future_time() ->
    Self = self(),
    CellPosition = {2, 2},
    {ok, _} = dgol:start_session_and_wait(5, 5, [CellPosition], 50),
    
    CellPid = cell_locator:get(CellPosition),

    cell:evolve_at(CellPid, 3),
    timer:sleep(500),
    cell:eventually_get(CellPid, 1, fun(Result) -> Self ! Result end),
    ?assertReceive({cell, {2, 2}, _ExpectedTime = 1, _ExpectedContent = 0}, 50),
    dgol:evolve_at(1),
    timer:sleep(500),
    cell:eventually_get(CellPid, 2, fun(Result) -> Self ! Result end),
    ?assertReceive({cell, {2, 2}, 2, 0}, 50),
    dgol:evolve_at(2),
    timer:sleep(500),
    cell:eventually_get(CellPid, 3, fun(Result) -> Self ! Result end),
    ?assertReceive({cell, {2, 2}, 3, 0}, 50).

cells_can_evolve_together() ->
    StepOneLivingCells = [{2, 1}, {2, 2}, {2, 3}],
    StepTwoLivingCells = [{1, 2}, {2, 2}, {3, 2}],
    {ok, _} = dgol:start_session_and_wait(5, 5, StepOneLivingCells, 50),

    ?assertMatch(
       [{_, _, _, 1}, {_, _, _, 1}, {_, _, _, 1}], 
       [cell:get(cell_locator:get(Position), 0) || Position <- StepOneLivingCells]),
    dgol:evolve_at(1),
    timer:sleep(500),
    ?assertMatch(
       [{_, _, _, 1}, {_, _, _, 1}, {_, _, _, 1}], 
       [cell:get(cell_locator:get(Position), 1) || Position <- StepTwoLivingCells]).

events_about_cells_are_emitted_correctly() ->
    {ok, _} = dgol:start_session_and_wait(5, 5, [{1,1}], 50),
    exit(cell_locator:get({1, 1}), test),
    timer:sleep(500),
    ?assert(recorder:is_recorded({cell_born, {1, 1}, 1}, 2)),
    ?assert(recorder:is_recorded({cell_died, {1, 1}})).

cell_register_itself_after_the_rebirth() ->
    {ok, _} = dgol:start_session_and_wait(5, 5, [{1,1}], 50),
    OldPid = cell_locator:get({1, 1}),
    exit(OldPid, test),
    timer:sleep(500),
    NewPid = cell_locator:get({1, 1}),
    ?assert(is_pid(NewPid)),
    ?assert(OldPid =/= NewPid).

cell_can_recover_after_death() ->
    {ok, _} = dgol:start_session_and_wait(5, 5, [], 50),
    dgol:evolve_at(5),
    ?assertEqual(5, dgol:target_time()),
    timer:sleep(500),
    ?assert(recorder:is_recorded({cell_evolved, {1, 1}, 0, 5}, 1)),
    exit(cell_locator:get({1, 1}), test),
    timer:sleep(500),
    ?assert(recorder:is_recorded({cell_evolved, {1, 1}, 0, 5}, 2)).

