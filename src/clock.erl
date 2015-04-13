-module(clock).
-behaviour(gen_event).

-include_lib("eunit/include/eunit.hrl").

-export([get_time/0]).
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         code_change/3,
         terminate/2
        ]).

-record(state, {time}).

-spec get_time() -> non_neg_integer().
get_time() ->
    gen_event:call(deb, clock, get_time).

init(_) ->
    {ok, #state{time=0}}.

handle_event({cell_evolved, _, _, Time}, State) when Time > State#state.time ->
    {ok, State#state{time=Time}};
handle_event(_, State) ->
    {ok, State}.

handle_call(get_time, State) ->
    {ok, State#state.time, State}.

handle_info(_Event, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_, _State) ->
    ok.

-ifdef(TEST).

clock_works_test() ->
    gen_event:start({local, deb}),
    gen_event:add_handler(deb, clock, []),

    ?assertEqual(0, clock:get_time()),
    gen_event:notify(deb, {cell_born, {1, 1}, 1, 0}),
    ?assertEqual(0, clock:get_time()),
    gen_event:notify(deb, {cell_evolved, {1, 1}, 1, 1}),
    ?assertEqual(1, clock:get_time()),
    gen_event:notify(deb, {cell_evolved, {1, 2}, 0, 1}),
    ?assertEqual(1, clock:get_time()),
    gen_event:notify(deb, {cell_evolved, {1, 1}, 0, 7}),
    ?assertEqual(7, clock:get_time()),
    gen_event:notify(deb, {cell_evolved, {1, 3}, 1, 1}),
    ?assertEqual(7, clock:get_time()),

    gen_event:stop(deb).

-endif.
