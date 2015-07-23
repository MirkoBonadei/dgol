-module(gui_server).
-behaviour(gen_server).
-include_lib("wx/include/wx.hrl").

-export([start/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {frame=nil,
                grid=nil,
                time=0}).


start() ->
    gen_server:start(?MODULE, [], []).

init(_) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, 
                        -1,
                        "Distributed game of life", 
                        [{size, {800, 670}}]),
    gen_event:add_handler(deb, gui, self()),
    {ok, #state{frame=Frame, 
                grid=nil, 
                time=0}}.

handle_call(_, _, _) ->
    {reply, ok, []}.

handle_cast({universe_created, X, Y}, S=#state{frame=F}) ->
    io:format(user, "universe_created~n", []),
    Sz = wxBoxSizer:new(?wxVERTICAL),
    G = wxGrid:new(F, -1),
    wxGrid:createGrid(G, X, Y),
    wxGrid:setColMinimalAcceptableWidth(G, 1),
    wxGrid:setRowMinimalAcceptableHeight(G, 1),
    [wxGrid:setColSize(G, Col, round(780/Y)) || Col <- lists:seq(0, Y)],
    [wxGrid:setRowSize(G, Row, round(580/X)) || Row <- lists:seq(0, X)],
    wxGrid:setRowLabelSize(G, 0),
    wxGrid:setColLabelSize(G, 0),
    wxGrid:enableEditing(G, false),
    wxGrid:disableDragGridSize(G),
    wxGrid:disableDragColSize(G),
    wxGrid:disableDragRowSize(G),

    wxFrame:createStatusBar(F),
    wxFrame:setStatusText(F, "Ready"),

    Hs = wxBoxSizer:new(?wxHORIZONTAL),
    Tick = wxButton:new(F, 10, [{label, "Tick"}]),
    Auto = wxButton:new(F, 11, [{label, "Start"}]),
    wxSizer:add(Hs, Tick, [{border, 4}]),
    wxSizer:add(Hs, Auto, [{border, 4}]),

    wxSizer:add(Sz, G, [{border, 4}]),
    wxSizer:add(Sz, Hs, [{border, 4}]),
    wxWindow:setSizer(F, Sz),
    wxGrid:forceRefresh(G),

    %%wxFrame:connect(F, command_button_clicked),
    %%wxFrame:connect(F, grid_cell_left_dclick),
    %%wxFrame:connect(F, close_window),

    wxFrame:show(F),
    {noreply, S#state{grid=G}};
handle_cast(E, _) ->
    io:format(user, "~p~n", [E]),
    {noreply, []}.

handle_info(_, _) ->
    {noreply, []}.

terminate(_, _) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
