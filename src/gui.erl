-module(gui).
-behaviour(gen_event).
-include_lib("wx/include/wx.hrl").

-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         code_change/3,
         terminate/2
        ]).

%% TODO:
%% - dovrebbe essere impossibile selezionare più di una cella (anche con il drag del mouse)
%% - non è il massimo andare sulla handle_info/2 per gestire gli eventi della GUI

init(_) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, -1, "Distributed game of life", [{size, {800, 670}}]),
    {ok, {Frame, nil, 1}}.

handle_event({universe_created, Xdim, Ydim}, {Frame, _, Time}) ->
    Sz = wxBoxSizer:new(?wxVERTICAL),
    Grid = wxGrid:new(Frame, -1),
    wxGrid:createGrid(Grid, Xdim, Ydim),
    wxGrid:setColMinimalAcceptableWidth(Grid, 1),
    wxGrid:setRowMinimalAcceptableHeight(Grid, 1),
    [wxGrid:setColSize(Grid, Col, round(780/Ydim)) || Col <- lists:seq(0, Ydim)],
    [wxGrid:setRowSize(Grid, Row, round(580/Xdim)) || Row <- lists:seq(0, Xdim)],
    wxGrid:setRowLabelSize(Grid, 0),
    wxGrid:setColLabelSize(Grid, 0),
    wxGrid:enableEditing(Grid, false),
    wxGrid:disableDragGridSize(Grid),
    wxGrid:disableDragColSize(Grid),
    wxGrid:disableDragRowSize(Grid),

    wxFrame:createStatusBar(Frame),
    wxFrame:setStatusText(Frame, "Ready"),

    Hs = wxBoxSizer:new(?wxHORIZONTAL),
    Tick = wxButton:new(Frame, 10, [{label, "Tick"}]),
    Auto = wxButton:new(Frame, 11, [{label, "Start"}]),
    wxSizer:add(Hs, Tick, [{border, 4}]),
    wxSizer:add(Hs, Auto, [{border, 4}]),

    wxSizer:add(Sz, Grid, [{border, 4}]),
    wxSizer:add(Sz, Hs, [{border, 4}]),
    wxWindow:setSizer(Frame, Sz),
    wxGrid:forceRefresh(Grid),

    wxFrame:connect(Frame, command_button_clicked),
    wxFrame:connect(Frame, grid_cell_left_dclick),

    wxFrame:show(Frame),
    {ok, {Frame, Grid, Time}};
handle_event({cell_born, {X, Y}, 1}, {Frame, Grid, Time}) ->
    wxGrid:setCellBackgroundColour(Grid, X, Y, {0, 0, 0, 0}),
    wxGrid:forceRefresh(Grid),
    {ok, {Frame, Grid, Time}};
handle_event({cell_evolved, {X, Y}, 0, _}, {Frame, Grid, Time}) ->
    wxGrid:setCellBackgroundColour(Grid, X, Y, {255, 255, 255, 0}),
    wxGrid:forceRefresh(Grid),
    {ok, {Frame, Grid, Time}};
handle_event({cell_evolved, {X, Y}, 1, _}, {Frame, Grid, Time}) ->
    wxGrid:setCellBackgroundColour(Grid, X, Y, {0, 0, 0, 0}),
    wxGrid:forceRefresh(Grid),
    {ok, {Frame, Grid, Time}};
handle_event({cell_died, {X, Y}}, {Frame, Grid, Time}) ->
    wxFrame:setStatusText(Frame, io_lib:format("Cell {~p, ~p} is dead", [X, Y])),
    {ok, {Frame, Grid, Time}};
handle_event({target_time_updated, Time}, {Frame, Grid, _}) ->
    wxFrame:setStatusText(Frame, io_lib:format("Time: ~p", [Time])),
    {ok, {Frame, Grid, Time + 1}};
handle_event(Event, State) ->
    io:format(user, "~p~n", [Event]),
    {ok, State}.

handle_call(_Event, State) ->
    %%io:format(user, "~p~n", [Event]),
    {ok, reply, State}.

handle_info(#wx{id=10, event=#wxCommand{type=command_button_clicked}}, {Frame, Grid, Time}) ->
    dgol:evolve_at(Time),
    {ok, {Frame, Grid, Time}};
handle_info(#wx{id=11, event=#wxCommand{type=command_button_clicked}}, {Frame, Grid, Time}) ->
    AutoButton = wx:typeCast(wxWindow:findWindowById(11), wxButton),
    TickButton = wx:typeCast(wxWindow:findWindowById(10), wxButton),
    case wxButton:getLabel(AutoButton) of
        "Start" ->  
            wxButton:setLabel(AutoButton, "Stop"),
            wxButton:disable(TickButton),
            start_timer(Time);
        "Stop" -> 
            wxButton:setLabel(AutoButton, "Start"),
            wxButton:enable(TickButton),
            stop_timer()
    end,
    {ok, {Frame, Grid, Time}};
handle_info(#wx{event=#wxGrid{type=grid_cell_left_dclick, row=X, col=Y}}, {Frame, Grid, Time}) ->
    exit(cell_locator:get({X, Y}), kill),
    {ok, {Frame, Grid, Time}};
handle_info(Event, State) ->
    io:format(user, "~p~n", [Event]),
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_, _Frame) ->
    wx:destroy(),
    ok.

start_timer(Time) ->
    register(ticker, spawn(fun() -> tick(Time, 500) end)).

stop_timer() ->
    exit(whereis(ticker), kill),
    unregister(ticker).

tick(Time, SleepTime) ->
    dgol:evolve_at(Time),
    timer:sleep(SleepTime),
    tick(Time + 1, SleepTime).
