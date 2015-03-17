-module(collector).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([start_link/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-record(state, { time :: cell:time(),
                 neighbours :: cell:neighbours()}).

-spec start_link(cell:time(), cell:neighbours()) -> {ok, pid()}.
start_link(TimeToCollect, Neighbours) ->
    gen_server:start_link(?MODULE,
                          #state{time=TimeToCollect,
                                 neighbours=Neighbours}, 
                          []).

init(State) ->
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-ifdef(TEST).



-endif.
