-module(demo).
-export([start/2,
         stop/0]).

start(Dimension, StartingCells) ->
    application:start(dgol),
    dgol:start_session(Dimension, 
                       Dimension, 
                       StartingCells,
                       [{gui, []}]).
    
stop() ->
    application:stop(dgol).
