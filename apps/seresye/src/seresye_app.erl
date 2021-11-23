%%%-------------------------------------------------------------------
%%% SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%%
%%% @author Francesca Gangemi
%%% @author Corrado Santoro
%%% @copyright (C) 2005-2010
%%%
%%% @copyright (C) 2011, <Afiniate, Inc.>
%%%
%%% Updated by,
%%% @copyright Miloud Eloumri, <miloud.eloumri@gmail.com>,
%%% [https://www.linkedin.com/in/miloudeloumri/], 16. Nov 2021 10:10 AM.
%%% Compiled with Rebar3 3.17.0 on Erlang/OTP 23 Erts 11.0
%%%
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
%%%
%%%-------------------------------------------------------------------
%% @doc seresye public API.
%%% @version 0.0.5.
%%% Updated : 16. Nov 2021 10:10 AM.
%%%
%%% seresye application behaviour provides
%%% an interface to start and stop involved applications as a whole
%%% loading all their modules and starting main processes
%%%
%% @end
%%%-------------------------------------------------------------------
-module(seresye_app).

-behaviour(application).

%% API
-export([start/0]).
%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% wrapper to start applications
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok.
start() ->
    ok = application:start(erlware_commons),
    ok = application:start(seresye_srv).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(StartType :: normal | {takeover, node()} | {failover, node()},
            StartArgs :: term()) ->
               {ok, pid()} | {ok, pid(), State :: term()} | {error, Reason :: term()}.
start(_StartType, _StartArgs) ->
    case seresye_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(State::term()) -> ok.
stop(_State) ->
    ok.

%% internal functions
