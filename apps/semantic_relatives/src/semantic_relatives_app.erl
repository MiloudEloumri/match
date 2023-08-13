%%%-------------------------------------------------------------------
%% @doc semantic_relatives public API
%% @end
%%%-------------------------------------------------------------------

-module(semantic_relatives_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    semantic_relatives_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
