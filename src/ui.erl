-module(ui).
-behaviour(gen_event).

-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         code_change/3,
         terminate/2
        ]).

init(_) ->
    {ok, []}.

handle_event(Event, State) ->
    io:format(user, "~p~n", [Event]),
    {ok, State}.

handle_call(_Event, State) ->
    {reply, ok, State}.

handle_info(_Event, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_, _State) ->
    ok.


