-module(gui_server).
-behaviour(gen_server).
-include_lib("wx/include/wx.hrl").
%% TODO:
%% - ascoltare la morte del ticker e fare il toggle dell'auto-button
%% - fare uno step di accensione delle celle (?)
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

%% TODO: how to disable syncronous messages?
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

    wxFrame:connect(F, command_button_clicked),
    wxFrame:connect(F, grid_cell_left_dclick),
    wxFrame:connect(F, close_window),

    wxFrame:show(F),
    {noreply, S#state{grid=G}};
handle_cast({cell_born, {X, Y}, _}, S=#state{grid=G,time=T}) when T > 1 ->
    wxGrid:setCellBackgroundColour(G, X, Y, {255, 0, 0, 0}),
    wxGrid:forceRefresh(G),
    {noreply, S};
handle_cast({cell_born, {X, Y}, 1}, S=#state{grid=G}) ->
    wxGrid:setCellBackgroundColour(G, X, Y, {0, 0, 0, 0}),
    wxGrid:forceRefresh(G),
    {noreply, S};
handle_cast({cell_evolved, {X, Y}, 0, T}, S=#state{grid=G,time=T}) ->
    wxGrid:setCellBackgroundColour(G, X, Y, {255, 255, 255, 0}),
    wxGrid:forceRefresh(G),
    {noreply, S};
handle_cast({cell_evolved, {X, Y}, 1, T}, S=#state{grid=G,time=T}) ->
    wxGrid:setCellBackgroundColour(G, X, Y, {0, 0, 0, 0}),
    wxGrid:forceRefresh(G),
    {noreply, S};
handle_cast({cell_died, {X, Y}}, S=#state{frame=F}) ->
    wxFrame:setStatusText(F, io_lib:format("Cell {~p, ~p} is dead", [X, Y])),
    {noreply, S};
handle_cast({target_time_updated, T}, S=#state{frame=F}) ->
    wxFrame:setStatusText(F, io_lib:format("Time: ~p", [T])),
    {noreply, S#state{time=T}};
handle_cast(_E, S) ->
    {noreply, S}.

handle_info(#wx{id=10,event=#wxCommand{type=command_button_clicked}}, S) ->
    dgol:evolve(),
    {noreply, S};
handle_info(#wx{id=11,event=#wxCommand{type=command_button_clicked}}, S) ->
    AutoButton = wx:typeCast(wxWindow:findWindowById(11), wxButton),
    TickButton = wx:typeCast(wxWindow:findWindowById(10), wxButton),
    case wxButton:getLabel(AutoButton) of
        "Start" ->  
            wxButton:setLabel(AutoButton, "Stop"),
            wxButton:disable(TickButton),
            start_timer();
        "Stop" -> 
            wxButton:setLabel(AutoButton, "Start"),
            wxButton:enable(TickButton),
            stop_timer()
    end,
    {noreply, S};
handle_info(#wx{event=#wxGrid{type=grid_cell_left_dclick,row=X,col=Y}}, S) ->
    exit(cell_locator:get({X, Y}), kill),
    {noreply, S};
handle_info(#wx{event=#wxClose{type=close_window}}, S=#state{frame=F}) ->
    application:stop(dgol),
    wxFrame:destroy(F),
    {noreply, S};
handle_info(_E, S) ->
    {noreply, S}.

terminate(_, _) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

start_timer() ->
    register(ticker, spawn(fun() -> tick(500) end)).

stop_timer() ->
    exit(whereis(ticker), kill),
    unregister(ticker).

tick(SleepTime) ->
    dgol:evolve(),
    timer:sleep(SleepTime),
    tick(SleepTime).
