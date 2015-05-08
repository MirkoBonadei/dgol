-module(endurance).

-export([start/1,
         stop/0,
         tick/2,
         troublemaker/2]).

start(Dimension) ->
    application:start(dgol),
    StartingCells = [{2, 2}, {3, 2}, {4, 2}],
    dgol:start_session(Dimension, 
                       Dimension, 
                       StartingCells),
    %%timer:sleep(10000),
    spawn_link(?MODULE, tick, [1, 200]).
    %%spawn_link(?MODULE, troublemaker, [Dimension, 1000]).
    
stop() ->
    application:stop(dgol).

tick(Time, SleepTime) ->
    dgol:evolve_at(Time),
    timer:sleep(SleepTime),
    tick(Time + 1, SleepTime).

troublemaker(Dimension, Time) ->
    timer:sleep(random:uniform(Time)),
    
    case cell_locator:get({random:uniform(Dimension - 1), 
                           random:uniform(Dimension - 1)}) of
        Pid when is_pid(Pid) ->
            exit(Pid, kill),
            troublemaker(Dimension, Time);
        {error, not_found} ->
            troublemaker(Dimension, Time)
    end.
