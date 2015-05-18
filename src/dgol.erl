-module(dgol).
-behaviour(gen_server).

-export([start_session/3,
         evolve_at/1,
         start_session_and_wait/4,
         target_time/0]).
-export([start_link/3]).
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

-record(state, {size_x :: pos_integer(),
                size_y :: pos_integer(),
                target_time :: non_neg_integer()}).

-spec start_session(pos_integer(), pos_integer(), [cell:position(), ...]) ->
                           supervisor:startchild_ret() | {error, already_started}.
start_session(Xdim, Ydim, InitialCells) ->
    case whereis(dgol) of
        undefined ->
            gen_event:notify(deb, {universe_created, Xdim, Ydim}),
            supervisor:start_child(dgol_sup, 
                                   {dgol, {dgol, start_link, [Xdim, Ydim, InitialCells]},
                                           permanent, 2000, worker, [dgol]});
        _ ->
            {error, already_started}
    end.

-spec start_session_and_wait(pos_integer(), pos_integer(), [cell:position(), ...], timeout()) ->
                           supervisor:startchild_ret() | {error, already_started} | {error, timeout}.
start_session_and_wait(XDim, YDim, InitialCells, Timeout) ->
    case dgol:start_session(XDim, YDim, InitialCells) of
        Error = {error, already_started} ->
            Error;
        SupervisorStartChildRet ->
            Positions = [{X, Y} || X <- lists:seq(0, XDim - 1),
                                   Y <- lists:seq(0, YDim - 1)],
            case cell_locator:wait_for_all(Positions, Timeout) of
                timeout ->
                    {error, timeout};
                ok ->
                    SupervisorStartChildRet
            end
    end.

-spec evolve_at(cell:time()) -> ok.
evolve_at(Time) ->
    gen_server:cast(?MODULE, {evolve_at, Time}).

-spec start_link(pos_integer(), pos_integer(), [cell:position(), ...]) -> 
                        {ok, pid()} | 
                        ignore | 
                        {error, {already_started, pid()} | term()}.
start_link(Xdim, Ydim, InitialCells) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Xdim, Ydim, InitialCells], []).

-spec target_time() -> non_neg_integer().
target_time() ->
    gen_server:call(?MODULE, target_time, 10000).

%%% OTP gen_server callbacks

init([Xdim, Ydim, InitialCells]) ->
    gen_server:cast(self(), {start_cells, Xdim, Ydim, InitialCells}),
    {ok, #state{size_x = Xdim,
                size_y = Ydim,
                target_time = 0}}.

handle_call(target_time, _From, State) ->
    {reply, State#state.target_time, State}.

handle_cast({start_cells, Xdim, Ydim, InitialCells}, State) ->
    DgolPid = self(),
    spawn(fun() -> start_cells(Xdim, Ydim, InitialCells, DgolPid) end),
    {noreply, State};
handle_cast(init_done, State) ->
    {noreply, State};
handle_cast({evolve_at, Time}, State) ->
    [cell:evolve_at(cell_locator:get({X, Y}), Time) || X <- lists:seq(0, State#state.size_x - 1),
                                                       Y <- lists:seq(0, State#state.size_y - 1)],
    {noreply, State#state{target_time=Time}}.

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

