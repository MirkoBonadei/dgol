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

init(_) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, -1, "Distributed game of life", [{size, {800, 600}}]),
    {ok, {Frame, nil}}.

handle_event({universe_created, Xdim, Ydim}, {Frame, _}) ->
    Grid = wxGrid:new(Frame, 10, 10),
    wxGrid:createGrid(Grid, Xdim, Ydim),
    wxGrid:setColMinimalAcceptableWidth(Grid, 1),
    wxGrid:setRowMinimalAcceptableHeight(Grid, 1),
    [wxGrid:setColSize(Grid, Col, round(780/ Ydim)) || Col <- lists:seq(0, Ydim)],
    [wxGrid:setRowSize(Grid, Row, round(580/Xdim)) || Row <- lists:seq(0, Xdim)],
    wxGrid:setRowLabelSize(Grid, 0),
    wxGrid:setColLabelSize(Grid, 0),
    wxGrid:enableEditing(Grid, false),
    wxGrid:disableDragGridSize(Grid),
    wxGrid:disableDragColSize(Grid),
    wxGrid:disableDragRowSize(Grid),
    %%[wxGrid:setColSize(Grid, X, ) || X <- lists:seq(0, Xdim), Y <- lists:seq(0, Ydim)],    
    wxGrid:forceRefresh(Grid),
    wxFrame:show(Frame),
    {ok, {Frame, Grid}};
handle_event({cell_born, {X, Y}, 1}, {Frame, Grid}) ->
    wxGrid:setCellBackgroundColour(Grid, X, Y, {0, 0, 0, 0}),
    wxGrid:forceRefresh(Grid),
    {ok, {Frame, Grid}};
handle_event(_, Frame) ->
    {ok, Frame}.

handle_call(_Event, State) ->
    {ok, reply, State}.

handle_info(_Event, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_, _Frame) ->
    wx:destroy(),
    ok.
