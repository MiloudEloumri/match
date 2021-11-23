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
%%% [https://www.linkedin.com/in/miloudeloumri/], 12. Nov 2021 9:10 PM.
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
%% @doc seresye top level supervisor.
%%% @version 0.0.5.
%%% Updated : 12. Nov 2021 9:10 PM.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(seresye_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_engine/0, start_engine/1, start_engine/2]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor.
%%
%% @end
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    Args = [],
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%% @doc Dynamically add a child specification to a supervisor SupRef,
%% which starts the corresponding child process.
%% When using, simple_one_for_one strategy,
% child specification defined in init/1 is used.
%% @end
start_engine() ->
    supervisor:start_child(?SERVER, []).

%% @doc Dynamically add a child specification to a supervisor SupRef,
%% which starts the corresponding child process.
%% When using, simple_one_for_one strategy,
% child specification defined in init/1 is used.
%% @end
start_engine(Name) ->
    supervisor:start_child(?SERVER, [Name]).

%% @doc Dynamically add a child specification to a supervisor SupRef,
%% which starts the corresponding child process.
%% When using, simple_one_for_one strategy,
% child specification defined in init/1 is used.
%% @end
start_engine(Name, ClientState) ->
    supervisor:start_child(?SERVER, [Name, ClientState]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% init function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
-spec init(Args :: term()) ->
              {ok,
               {SupFlags ::
                    {RestartStrategy :: supervisor:strategy(),
                     MaxR :: non_neg_integer(),
                     MaxT :: non_neg_integer()},
                [ChildSpec :: supervisor:child_spec()]}} |
              ignore |
              {error, Reason :: term()}.
init(_Args) ->
    %% process_flag(trap_exit, true),
    %% SupFlags are supervisor, flags, properties or configurations represented as a map
    %% Configuration options common to all child processes of this supervisor.
    %% If a child process crashes, restart only that one (simple_one_for_one).
    %% If there is more than 1000 crash ('intensity') in
    %% 3600 seconds ('period'), the entire supervisor crashes
    %% with all its children.
    RestartStrategy =
        simple_one_for_one, % dynamically added or removed child processes at runtime; all of the same type and running the same code
    MaxRestarts =
        1000, % maximum 1000 child restarts (intensity) are allowed within maximum time seconds (period)
    MaxSecondsBetweenRestarts =
        3600, % 1000 restarts (period) are allowed in 3600 seconds (1 hour)
    SupFlags =
        #{strategy => RestartStrategy,
          intensity => MaxRestarts,
          period => MaxSecondsBetweenRestarts},

    %% child processes specification list map
    ChildSpecs = [child(seresye_srv)],

    %% Return the supervisor flags and the child specifications
    %% to the supervisor module.
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc
%% Returns a child specifications map.
%%
%% @end
-spec child(Module :: term()) -> map().
child(Module) ->
    Id =
        Module, % mandatory - unique identifier internal to this supervisor used by the supervisor to identify the child
    StartFunction =
        {Module,
         start_link,
         []}, % mandatory - defines the function call used to start the child process and links, spawns the child process
    RestartType = transient,   % restart a child only on abnormal termination
    ShutDown =
        2000, % maximum time in milliseconds to wait for a child to terminate (time-out)
    ChildType = worker, % a child process type can be supervisor or  worker
    Modules = [Module], % a list of call back modules implementing a child

    %% Return child specification map
    #{id => Id,
      start => StartFunction,
      restart => RestartType,
      shutdown => ShutDown,
      type => ChildType,
      modules => Modules}.
