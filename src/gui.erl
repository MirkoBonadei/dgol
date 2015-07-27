-module(gui).
-behaviour(gen_event).
-export([init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         code_change/3,
         terminate/2
        ]).
-record(state, {gui_server :: pid()}).

-spec init(GuiServer :: pid()) -> {ok, term()}. %% fix this
init(GuiServer) ->
    {ok, #state{gui_server=GuiServer}}.

handle_event(E={universe_created, _X, _Y}, S=#state{gui_server=Pid}) ->
    gen_server:cast(Pid, E),
    {ok, S};
handle_event(E={cell_born, {_X, _Y}, _}, S=#state{gui_server=Pid}) ->
    gen_server:cast(Pid, E),
    {ok, S};
handle_event(E={cell_evolved, {_X, _Y}, _C, _T}, S=#state{gui_server=Pid}) ->
    gen_server:cast(Pid, E),
    {ok, S};
handle_event(E={cell_died, {_X, _Y}}, S=#state{gui_server=Pid}) ->
    gen_server:cast(Pid, E),
    {ok, S};
handle_event(E={target_time_updated, _T}, S=#state{gui_server=Pid}) ->
    gen_server:cast(Pid, E),
    {ok, S};
handle_event(_E, S) ->
    {ok, S}.

handle_call(_E, S) ->
    {ok, ok, S}.

handle_info(_E, S) ->
    {ok, S}.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

terminate(_, _S) ->
    ok.
