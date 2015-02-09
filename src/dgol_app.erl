-module(dgol_app).

-behaviour(application).

-export([start/2
        ,stop/1]).

start(_StartType, _StartArgs) ->
    %% TODO: is a check here useful or not (see OTP in Action page 124)
    dgol_sup:start_link().

stop(_State) ->
    ok.

