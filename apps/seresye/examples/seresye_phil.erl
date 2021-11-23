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
%%% @doc seresye Philosophers Problem
%%% https://en.wikipedia.org/wiki/Dining_philosophers_problem
%%% @version 0.0.5.
%%% Updated : 17. Nov 2021 8:11 AM.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(seresye_phil).

-export([start/0, phil_spawn/1, philosopher/2, think/1, eat/1]).

-define(N_PHIL, 5).

start() ->
  seresye_srv:start(restaurant),
  phil_spawn(0).

phil_spawn(?N_PHIL) -> ok;
phil_spawn(N) ->
  seresye_srv:assert(restaurant, {fork, N}),
  spawn(seresye_phil, philosopher, [N, init]),
  if
    N < (?N_PHIL - 1) ->
      seresye_srv:assert(restaurant, {room_ticket, N});
    true ->
      ok
  end,
  phil_spawn(N + 1).

philosopher(N, init) ->
  new_seed(),
  philosopher(N, ok);
philosopher(N, X) ->
  think(N),
  Ticket = seresye_srv:retract(restaurant, {room_ticket, '_'}),
  seresye_srv:retract(restaurant, {fork, N}),
  seresye_srv:retract(restaurant, {fork, (N + 1) rem ?N_PHIL}),
  eat(N),
  seresye_srv:assert(restaurant, {fork, N}),
  seresye_srv:assert(restaurant, {fork, (N + 1) rem ?N_PHIL}),
  seresye_srv:assert(restaurant, Ticket),
  philosopher(N, X).

think(N) ->
  io:format("~w: thinking ...~n", [N]),
  timer:sleep(rand:uniform(10) * 1000).

eat(N) ->
  io:format("~w: eating ...~n", [N]),
  timer:sleep(rand:uniform(10) * 1000).


new_seed() ->
  {_, _, X} = erlang:timestamp(),
  {H, M, S} = time(),
  H1 = H * X rem 32767,
  M1 = M * X rem 32767,
  S1 = S * X rem 32767,
  put(random_seed, {H1, M1, S1}).