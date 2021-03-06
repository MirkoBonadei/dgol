-module(recorder).
-behaviour(gen_event).

-export([is_recorded/1,
         is_recorded/2]).
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         code_change/3,
         terminate/2
        ]).

is_recorded(Event) ->
    gen_event:call(deb, recorder, {is_recorded, Event}).

is_recorded(Event, Times) ->
    gen_event:call(deb, recorder, {is_recorded, Event, Times}).

init(_) ->
    {ok, []}.

handle_event(Event, State) ->
    {ok, [Event|State]}.

handle_call({is_recorded, Event}, State) ->
    {ok, lists:member(Event, State), State};
handle_call({is_recorded, Event, Times}, State) ->
    Occurrencies = lists:foldl(fun(E, Acc) when E =:= Event -> Acc + 1;
                                  (_, Acc) -> Acc 
                               end, 0, State),
    {ok, Occurrencies =:= Times, State}.

handle_info(_Event, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_, _State) ->
    ok.
