-module(demo).
-export([start/1,
         stop/0]).

%% TODO:
%% - far passare le celle dall'esterno

start(Dimension) ->
    application:start(dgol),
    StartingCells = [{2, 2}, {3, 2}, {4, 2}],
    dgol:start_session(Dimension, 
                       Dimension, 
                       StartingCells,
                       [{gui, []}]).
    
    
stop() ->
    application:stop(dgol).
