-module(universe).
-behaviour(gen_server).

-export([evolve/0,
         evolve_at/1,
         target_time/0]).
-export([start_link/2]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {size_x :: pos_integer(),
                size_y :: pos_integer(),
                target_time :: non_neg_integer()}).

-spec evolve_at(cell:time()) -> ok.
evolve_at(Time) ->
    gen_server:cast(?MODULE, {evolve_at, Time}).

-spec evolve() -> ok.
evolve() ->
    gen_server:cast(?MODULE, evolve).

-spec start_link(cell:dimensions(), [cell:position(), ...]) ->
                        {ok, pid()} |
                        ignore |
                        {error, {already_started, pid()} | term()}.
start_link({Xd, Yd}, InitialCells) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Xd, Yd, InitialCells], []).

-spec target_time() -> non_neg_integer().
target_time() ->
    gen_server:call(?MODULE, target_time, 10000).

%%% OTP gen_server callbacks

init([Xd, Yd, InitialCells]) ->
    gen_server:cast(self(), {start_cells, Xd, Yd, InitialCells}),
    % TODO: maybe after init?
    gen_event:notify(deb, {universe_created, Xd, Yd}),
    {ok, #state{size_x = Xd,
                size_y = Yd,
                target_time = 0}}.

handle_call(target_time, _From, State) ->
    {reply, State#state.target_time, State}.

handle_cast({start_cells, Xd, Yd, InitialCells}, State) ->
    DgolPid = self(),
    spawn(fun() -> start_cells(Xd, Yd, InitialCells, DgolPid) end),
    {noreply, State};
handle_cast(init_done, State) ->
    {noreply, State};
handle_cast({evolve_at, Time}, State) ->
    gen_event:notify(deb, {target_time_updated, Time}),
    [cell:evolve_at(cell_locator:get({X, Y}), Time) || X <- lists:seq(0, State#state.size_x - 1),
                                                       Y <- lists:seq(0, State#state.size_y - 1)],
    {noreply, State#state{target_time=Time}};
handle_cast(evolve, State) ->
    dgol:evolve_at(State#state.target_time + 1),
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%% private functions

-spec start_cells(pos_integer(), pos_integer(), [cell:position(), ...], pid()) -> ok.
start_cells(Xd, Yd, InitialCells, DgolPid) ->
    AllCells = [{X, Y} || X <- lists:seq(0, Xd - 1),
                          Y <- lists:seq(0, Yd - 1)],
    lists:foreach(fun(Pos) ->
                          start_cell(Pos, {Xd, Yd}, lists:member(Pos, InitialCells))
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

