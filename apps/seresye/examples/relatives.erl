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
%%% @doc seresye relatives example.
%%%
%%% @version 0.0.5.
%%% Updated : 17. Nov 2021 8:11 AM.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(relatives).
-export([father/3, grandfather/3, grandmother/3,
  mother/3, brother/4, sister/4, start/0]).

-rules([mother, father, brother, sister, grandfather,
  grandmother]).

%%
%% if (X is female) and (X is Y's parent) then (X is Y's mother)
%%
mother(Engine, {female, X}, {parent, X, Y}) ->
  seresye_engine:assert(Engine, {mother, X, Y}).

%%
%% if (X is male) and (X is Y's parent) then (X is Y's father)
%%
father(Engine, {male, X}, {parent, X, Y}) ->
  seresye_engine:assert(Engine, {father, X, Y}).

%%
%% if (Y and Z have the same parent X) and (Z is female)
%%    then (Z is Y's sister)
%%
sister(Engine, {parent, X, Y}, {parent, X, Z}, {female, Z}) when Y =/= Z ->
  seresye_engine:assert(Engine, {sister, Z, Y}).

%%
%% if (Y and Z have the same parent X) and (Z is male)
%%    then (Z is Y's brother)
%%
brother(Engine, {parent, X, Y}, {parent, X, Z}, {male, Z}) when Y =/= Z ->
  seresye_engine:assert(Engine, {brother, Z, Y}).

%%
%% if (X is Y's father) and (Y is Z's parent)
%%    then (X is Z's grandfather)
%%
grandfather(Engine, {father, X, Y}, {parent, Y, Z}) ->
  seresye_engine:assert(Engine, {grandfather, X, Z}).

%%
%% if (X is Y's mother) and (Y is Z's parent)
%%    then (X is Z's grandmother)
%%
grandmother(Engine, {mother, X, Y}, {parent, Y, Z}) ->
  seresye_engine:assert(Engine, {grandmother, X, Z}).

start() ->
  %% application:start(seresye_srv),
  seresye_srv:start(relatives),
  seresye_srv:add_rules(relatives, ?MODULE),

  seresye_srv:assert(relatives,
    [{male, bob},
      {male, corrado},
      {male, mark},
      {male, caesar},
      {female, alice},
      {female, sara},
      {female, jane},
      {female, anna},
      {parent, jane, bob},
      {parent, corrado, bob},
      {parent, jane, mark},
      {parent, corrado, mark},
      {parent, jane, alice},
      {parent, corrado, alice},
      {parent, bob, caesar},
      {parent, bob, anna},
      {parent, sara, casear},
      {parent, sara, anna}]),
  ok.