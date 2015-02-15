-module(dgol).
-behaviour(gen_server).

-export([start_link/3]).
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

-record(state, {size_x :: pos_integer(),
                size_y :: pos_integer()}).

-spec start_link(pos_integer(), pos_integer(), [cell:position(), ...]) -> 
                        {ok, pid()} | 
                        ignore | 
                        {error, {already_started, pid()} | term()}.
start_link(Xdim, Ydim, InitialCells) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Xdim, Ydim, InitialCells], []).

%%% OTP gen_server callbacks

init([Xdim, Ydim, InitialCells]) ->
    gen_server:cast(self(), {start_cells, Xdim, Ydim, InitialCells}),
    {ok, #state{size_x = Xdim,
                size_y = Ydim}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({start_cells, Xdim, Ydim, InitialCells}, State) ->
    DgolPid = self(),
    spawn(fun() -> start_cells(Xdim, Ydim, InitialCells, DgolPid) end),
    {noreply, State};
handle_cast(init_done, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%% private functions

-spec start_cells(pos_integer(), pos_integer(), [cell:position(), ...], pid()) -> ok.
start_cells(Xdim, Ydim, InitialCells, DgolPid) ->
    AllCells = [{X, Y} || X <- lists:seq(0, Xdim - 1),
                          Y <- lists:seq(0, Ydim - 1)],
    lists:foreach(fun(Pos) ->
                          start_cell(Pos, {Xdim, Ydim}, lists:member(Pos, InitialCells))
                  end,
                  AllCells),
    gen_server:cast(DgolPid, init_done).

-spec start_cell(cell:position(), cell:dimensions(), boolean()) -> pid().
start_cell(Position, Dimensions, true) ->
    {ok, Pid} = cell_sup:start_cell(Position, Dimensions, 1),
    Pid;
start_cell(Position, Dimensions, false) ->
    {ok, Pid} = cell_sup:start_cell(Position, Dimensions, 0),
    Pid.

