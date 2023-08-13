%%%-------------------------------------------------------------------
%%%
%%% SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%% https://github.com/gleber/seresye/tree/master
%%% 
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% Copyright (c) 2011, Afiniate, Inc.
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
%%%
%%%-------------------------------------------------------------------
%%%
%%% Semantic Web ToolKit for Erlang applications
%%% https://github.com/fogfish/semantic/tree/master
%%%
%%% Copyright 2012 - 2016 Dmitry Kolesnikov, All Rights Reserved
%%% Copyright 2016 Mario Cardona, All Rights Reserved
%%% 
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%% 
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%% 
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%-------------------------------------------------------------------
%%%
%%% SERESYE - Semantic relatives example created by:
%%% Miloud Eloumri, <miloud.eloumri@gmail.com>,
%%% Compiled with rebar 3.20.0 on Erlang/OTP 23 Erts 11.0
%%%
%%% https://github.com/MiloudEloumri/seresye
%%% https://github.com/fogfish/semantic/tree/master
%%%
%%%-------------------------------------------------------------------
%%%
%%%-------------------------------------------------------------------
%%% @doc SERESYE - Semantic relatives example.
%%% @version 0.0.5.
%%% Updated : 06. June 2023 7:30 PM.
%%%
%%% @end
%%%-------------------------------------------------------------------
%%%
-module(semantic_relatives_mnesia).

-include("semantic_relatives_mnesia_kb.hrl").

-export([father/3, grandfather/3, grandmother/3, mother/3, brother/4, sister/4, start/0]).

-rules([mother, father, brother, sister, grandfather,grandmother]).

%% @doc Determines if X is Y's mother given that X is female and X is Y's parent.
%% @end
mother(Engine, {female, X}, {parent, X, Y}) ->
  seresye_engine:set_client_state(seresye_engine:assert(Engine, {mother, X, Y}),
    [{mother, X, Y} | seresye_engine:get_client_state(Engine)]).

%% @doc Determines if X is Y's father given that X is male and X is Y's parent.
%% @end
father(Engine, {male, X}, {parent, X, Y}) ->
  seresye_engine:set_client_state(seresye_engine:assert(Engine, {father, X, Y}),
    [{father, X, Y} | seresye_engine:get_client_state(Engine)]).

%% @doc Determines if Z is Y's sister given that Y and Z have the same parent X and Z is female.
%% @end
sister(Engine, {parent, X, Y}, {parent, X, Z}, {female, Z}) when Y =/= Z ->
  seresye_engine:set_client_state(seresye_engine:assert(Engine, {sister, Z, Y}),
    [{sister, Z, Y} | seresye_engine:get_client_state(Engine)]).

%% @doc Determines if Z is Y's brother given that Y and Z have the same parent X and Z is male.
%% @end
brother(Engine, {parent, X, Y}, {parent, X, Z}, {male, Z}) when Y =/= Z ->
  seresye_engine:set_client_state(seresye_engine:assert(Engine, {brother, Z, Y}),
    [{brother, Z, Y} | seresye_engine:get_client_state(Engine)]).

%% @doc Determines if X is Z's grandfather given that X is Y's father and Y is Z's parent.
%% @end
grandfather(Engine, {father, X, Y}, {parent, Y, Z}) ->
  seresye_engine:set_client_state(seresye_engine:assert(Engine, {grandfather, X, Z}),
    [{grandfather, X, Z} | seresye_engine:get_client_state(Engine)]).

%% @doc Determines if X is Z's grandmother given that X is Y's mother and Y is Z's parent.
%% @end
grandmother(Engine, {mother, X, Y}, {parent, Y, Z}) ->
  seresye_engine:set_client_state(seresye_engine:assert(Engine, {grandmother, X, Z}),
    [{grandmother, X, Z} | seresye_engine:get_client_state(Engine)]).
      
%% @doc Starts the semantic library, loads a data file, starts the seresye server, adds rules,
%% extracts triples from the data file, and asserts them in the seresye server.
%% @end
-spec start() -> ok | error.
start() ->
  %% @doc Creates Mnesia Table.
  %% @end  
  case semantic_relatives_mnesia_handler:start() of
    ok -> ok;
    _ -> io:format("Could not start Mnesia handler. Mnesia operations will not be performed~n")      
  end,

  % Start the semantic library. If it starts successfully, continue; otherwise, print an error message.
  case semantic:start() of
    % The semantic module started successfully.
    {ok, []} ->
      % Join the given path elements into a path to a data file.
      NTriples = filename:join([code:priv_dir(semantic_relatives), "data", "ntriples-relatives.nt"]),
      % Load the data file into a list.
      NTriplesStream  = stream:list(semantic:nt(NTriples)),
      % Type the loaded data.
      NTriplesStreamTyped  =  semantic:typed(NTriplesStream),
      % Start the seresye server with a given name.
      seresye_srv:start(semantic_relatives_mnesia),
      % Add rules from the current module to the seresye server.
      seresye_srv:add_rules(semantic_relatives_mnesia, ?MODULE),
      % Extract tuples from the typed data.
      % There are two tuples patterns: {predicate, object, Subject} and {object, Subject}
      ListOfTuples = extract_tuples(NTriplesStreamTyped),
      % Assert each triple in the seresye server.
      lists:foreach(fun(Fact) ->  seresye_srv:assert(semantic_relatives_mnesia, Fact) end, ListOfTuples),
      % KB holds all facts in a list of tuples including the facts resulted from rules such as {mother, X, Y}
      % So KB has the following pattern: [{predicate, object, subject}, {object, subject}]
      KB = seresye_srv:get_kb(semantic_relatives_mnesia),
      % Store KB in Mnesia table
      case semantic_relatives_mnesia_handler:insert_kb(KB) of
        % KB stored in Mnesia table successfully.
        ok -> ok;
        % Error storing KB in Mnesia table.
        {error, Reason} -> 
          io:format("Error storing KB in Mnesia table: ~p~n", [Reason])
      end,
      ok;
    % The semantic module did not start successfully. Print an error message.
    {error, Reason} ->
      io:format("Error starting semantic: ~p~n", [Reason]),
      error
  end.

%% @doc Convert each map in the semantic data into a list of triples, depending on the predicate and object.
%% The match_map/1 function returns a list of triples for each map.
%% @end
-spec match_map(Map :: map()) -> [{atom(), atom(), atom()}].
match_map(Map) ->
  % Pattern match and assign values of s,p,o in a map to  S (Subject), P (predicate), and O (Object) variables respectively.
  #{s := S, p := P, o := O} = Map,
  % Check the predicate P.
  case P of
    % The predicate is "hasParent".
    {iri,<<"http://www.semanticweb.org/miloud/ontologies/2023/4/relatives#hasParent">>} ->
      Subject = get_name(S),
      Predicate = parent,
      Object = get_name(O),
      % return:
      [{Predicate, Object, Subject}];
    % The predicate is "type".
    {iri,<<"rdf">>,<<"type">>} ->
      % Check the object.
      case O of
        % The object is "Female".
        {iri,<<"http://www.semanticweb.org/miloud/ontologies/2023/4/relatives#Female">>} ->
          Subject = get_name(S),
          Object = female,
          % return:
          [{Object, Subject}];
        % The object is "Male".
        {iri,<<"http://www.semanticweb.org/miloud/ontologies/2023/4/relatives#Male">>} ->
          Subject = get_name(S),
          Object = male,
          % return:
          [{Object, Subject}];
        % The object is something else we are not interested in. Return an empty list.
        _ -> []
      end;
    % The predicate is something else we are not interested in. Return an empty list.
    _ -> []
  end.

%% @doc Pattern match either S or O saving their value in IRI variable.
%% Convert binary to list.
%% Extract the name from an IRI by splitting at # and taking the part after it.
%% Pattern match the result of the split - two lists and
%% we are only interested in the second list that starts with #.
%% The $ operator is used to get the ASCII value of #.
%% Return Name value after converting it to atom which gets bound to either Subject or Object.
%% @end
-spec get_name(IRI :: {atom(), binary()}) -> atom().
get_name({iri, IRI}) ->
  case lists:splitwith(fun(C) -> C /= $# end, binary_to_list(IRI)) of
    {_, "#" ++ Name} -> list_to_atom(Name)
  end.

%% @doc Extract triples from a list of maps by applying match_map to each map and flattening the result.
%% The lists:flatmap/2 function is a combination of map and flatten. It applies the provided function
%% (in this case match_map ) to each element of the list (each map in ListOfMaps), and then flattens the
%% results into a single list. The match_map function returns a list of triples for each map, so without
%% flattening we would end up with a list of lists of triples. But we want a single list of all the triples,
%% which is why we use flatmap instead of just map.
%% The flatmap would return [Triple1a, Triple1b, Triple2a, Triple2b, Triple3a].
%% @end
-spec extract_tuples(ListOfMaps :: [map()]) -> [{atom(), atom(), atom()}].
extract_tuples(ListOfMaps) ->
  lists:flatmap(fun match_map/1, ListOfMaps).