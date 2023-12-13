%%%-------------------------------------------------------------------
%% @doc job_match public API
%% @end
%%%-------------------------------------------------------------------

-module(job_match_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    job_match_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
