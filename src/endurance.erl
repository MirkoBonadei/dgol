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
                       StartingCells,
                       [{ui, []}]),
    %%timer:sleep(10000),
    spawn_link(?MODULE, tick, [1, 2000]),
    TroublemakerPid = spawn(?MODULE, troublemaker, [Dimension, 10000]),
    io:format("Troublemaker PID: ~p~n", [TroublemakerPid]).
    
stop() ->
    application:stop(dgol).

tick(Time, SleepTime) ->
    dgol:evolve_at(Time),
    timer:sleep(SleepTime),
    tick(Time + 1, SleepTime).

troublemaker(Dimension, Time) ->
    timer:sleep(Time),
    PositionToKill = {random:uniform(Dimension - 1), 
                      random:uniform(Dimension - 1)},
    case cell_locator:get(PositionToKill) of
        Pid when is_pid(Pid) ->
            exit(Pid, kill),
            file:write_file("/tmp/troublemaker",
                    list_to_binary(io_lib:format("Troublemaker: killed ~p~n", [PositionToKill])),
                    [append]),
            troublemaker(Dimension, Time);
        {error, not_found} ->
            file:write_file("/tmp/troublemaker",
                    list_to_binary(io_lib:format("Troublemaker: skip over ~p~n", [PositionToKill])),
                    [append]),
            troublemaker(Dimension, Time)
    end.
