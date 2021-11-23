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
%%% [https://www.linkedin.com/in/miloudeloumri/], 17. Nov 2021 8:11 AM.
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
%%% @doc seresye Producer–consumer Problem.
%%% https://en.wikipedia.org/wiki/Producer–consumer_problem
%%% @version 0.0.5.
%%% Updated : 17. Nov 2021 8:11 AM.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(seresye_prodcons).

-export([start/0, prod/1, cons/1, cons_1/1]).

start() ->
  application:start(seresye_srv),
  seresye_srv:start(pc),
  spawn(seresye_prodcons, cons_1, [1]),
  spawn(seresye_prodcons, cons_1, [2]),
  spawn(seresye_prodcons, cons_1, [3]),
  spawn(seresye_prodcons, prod, [0]),
  ok.

prod(20) -> ok;
prod(Index) ->
  seresye_srv:assert(pc, {item, Index}), prod(Index + 1).

cons(20) -> ok;
cons(Index) ->
  Fact = seresye_srv:retract(pc,
    {item, fun(X) -> X == Index end}),
  io:format("Consumer ~p~n", [Fact]),
  cons(Index + 1).

cons_1(N) ->
  Fact = seresye_srv:retract(pc, {item, '_'}),
  io:format("~w: Consumer ~p~n", [N, Fact]),
  timer:sleep(rand:uniform(500)),
  cons_1(N).
