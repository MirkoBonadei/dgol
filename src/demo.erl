-module(demo).
-export([start/2,
         stop/0]).

start(Dimension, StartingCells) ->
    application:start(dgol),
    gui_server:start(),
    %% TODO: remove the 4th parameter from the start_session function
    %%       it seems that it is now useless
    dgol:start_session(Dimension, 
                       Dimension, 
                       StartingCells,
                       []).
    
stop() ->
    application:stop(dgol).
