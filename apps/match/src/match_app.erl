%%%-------------------------------------------------------------------
%% @doc match public API
%% @end
%%%-------------------------------------------------------------------

-module(match_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    match_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
