-module(demo).
-export([start/1,
         stop/0,
         tick/2]).

start(Dimension) ->
    application:start(dgol),
    StartingCells = [{2, 2}, {3, 2}, {4, 2}],
    dgol:start_session(Dimension, 
                       Dimension, 
                       StartingCells),
    spawn_link(?MODULE, tick, [1, 2000]).
    
    
stop() ->
    application:stop(dgol).

tick(Time, SleepTime) ->
    dgol:evolve_at(Time),
    timer:sleep(SleepTime),
    tick(Time + 1, SleepTime).
