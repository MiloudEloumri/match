---
layout: default 
title: Swarm oriented ERlang Expert SYstem Engine
---

Build
-----

    $ rebar3 compile
    $ rebar3 shell

Introduction
------------

SERESYE is a Rete based rules engine written in Erlang, descended directly from the ERESYE project by Francesca Gangemi
and Corrado Santoro. In the following article we will describe how to use the system.

As it is widely known, a rule-based system is composed by a
**knowledge base**, which stores a set of *facts* representing the
'universe of discourse' of a given application, and a set of
**production rules**, which are used to infer knowledge and/or reason about the knowledge. A rule is activated when one
or more facts match the template(s) given in the rule declaration: in such a case, the body of the rule contains a code
that is thus executed

In SERESYE, *facts* are expressed by means of Erlang tuples or records, while rules are written using standard Erlang
function clauses, whose declaration reports, in the clause head, the facts or fact templates that have to be matched for
the rule to be activated and executed.

For more information about SERESYE please refer to the paper docs directory.

For more information about rule-based inference engines and expert systems, you can refer to the book: *S. Russell and
P. Norvig. **Artificial Intelligence: A Modern Approach/2E.** Prentice Hall, 2003.*

To write an AI application with SERESYE the following steps have to be performed:

1. Identify your universe of discourse and determine the facts that have to be used to represent such a world;

2. Identify the rules that you need and write them by using, e.g. first-order-logic predicates or even natural language;

3. Implement the system by writing your rules as Erlang function clauses, according to the modality required by SERESYE.

The Application: the Domain of Relatives
----------------------------------------

We will design a system able to derive new knowledge using some inference rules and starting from a small set; as a
sample application, we chose the domain of relatives: we will start from some base concepts, such as *parent*, *male*
and *female*, and then, by means of a proper set of rules, we will derive the concepts of
*mother*, *father*, *sister*, *brother*, *grandmother* and
*grandfather*.

According to the list above, we will first derive the facts that will be used to represent our concepts. Given the set
of relationships above, they will be represented by means of the following facts:

<table style="border:1px solid black;margin-left:auto;margin-right:auto;">
<thead>
  <tr>
    <td>#</td>
    <td>Concept</td>
    <td>Fact / Erlang tuple</td>
  </tr>
</thead>
<tbody>
  <tr>
    <td>1</td>
    <td>X is male</td>
    <td><code>{male, X}</code></td>
  </tr>
  <tr>
    <td>2</td>
    <td>X is female</td>
    <td><code>{female, X}</code></td>
  </tr>
  <tr>
    <td>3</td>
    <td>X is Y's parent</td>
    <td><code>{parent, X, Y}</code></td>
  </tr>
  <tr>
    <td>4</td>
    <td>X is Y's mother</td>
    <td><code>{mother, X, Y}</code></td>
  </tr>
  <tr>
    <td>5</td>
    <td>X is Y's father</td>
    <td><code>{father, X, Y}</code></td>
  </tr>
  <tr>
    <td>6</td>
    <td>X is Y's sister</td>
    <td><code>{sister, X, Y}</code></td>
  </tr>
  <tr>
    <td>7</td>
    <td>X is Y's brother</td>
    <td><code>{brother, X, Y}</code></td>
  </tr>
  <tr>
    <td>8</td>
    <td>X is Y's grandmother</td>
    <td><code>{grandmother, X, Y}</code></td>
  </tr>
  <tr>
    <td>9</td>
    <td>X is Y's grandfather</td>
    <td><code>{grandfather, X, Y}</code></td>
  </tr>
</tbody>
</table>

*Concepts 1, 2 and 3 will be used as a base to derive the other ones.*

Deriving new concepts by means of rules
---------------------------------------

#### Concept: mother

The rule to derive the concept of mother is quite straightforward:

    if X is female and X is Y's parent then X is Y's mother.

From the point of view of SERESYE, since knowledge is stored in the
*knowledge base* of the engine, the rule above is translated into the following one: *if the facts {female, X} and
{parent, X, Y} are
*asserted* in the knowledge base, then we assert the fact {mother, X, Y}.

The rule *mother* can be thus written as follows:

    %%
    %% if (X is female) and (X is Y's parent) then (X is Y's mother)
    %%
    mother (Engine, {female, X}, {parent, X, Y}) ->
      seresye:assert (Engine, {mother, X, Y}).

#### Concept: father

This concept can be easily derived by means of the following rule:

    %%
    %% if (X is male) and (X is Y's parent) then (X is Y's father)
    %%
    father (Engine, {male, X}, {parent, X, Y}) ->
      seresye:assert (Engine, {father, X, Y}).

#### Concept: sister

This concept can be expressed by the following rule:

    if Y and Z have the same parent and Z is female, then Z
    is the Y's sister.

The SERESYE rule used to map this concept is:

    %%
    %% if (Y and Z have the same parent X) and (Z is female)
    %%    then (Z is Y's sister)
    %%
    sister (Engine, {parent, X, Y}, {parent, X, Z}, {female, Z}) when Y =/= Z ->
      seresye:assert (Engine, {sister, Z, Y}).

Please note the guard, which is needed to ensure that when Y and Z are bound to the same value, the rule is not
activated (indeed this is possible since the same fact can match both the first and second
'parent' pattern).

#### Concept: brother

Given the previous one, this concept is now quite simple to implement:

    %%
    %% if (Y and Z have the same parent X) and (Z is male)
    %%    then (Z is Y's brother)
    %%
    brother (Engine, {parent, X, Y}, {parent, X, Z}, {male, Z}) when Y =/= Z ->
      seresye:assert (Engine, {brother, Z, Y}).

#### Concepts: grandmother and grandfather

The former concept can be expressed by means of the rule:

    if X is Y's mother and Y is Z's parent, then X is Z's
    grandmother.</u>* The latter concept is now obvious.

Both can be implemented using the following SERESYE rules:

    %%
    %% if (X is Y's mother) and (Y is Z's parent)
    %%    then (X is Z's grandmother)
    %%
    grandmother (Engine, {mother, X, Y}, {parent, Y, Z}) ->
      seresye:assert (Engine, {grandmother, X, Z}).

    %%
    %% if (X is Y's father) and (Y is Z's parent)
    %%    then (X is Z's grandfather)
    %%
    grandfather (Engine, {father, X, Y}, {parent, Y, Z}) ->
      seresye:assert (Engine, {grandfather, X, Z}).

Instantiating the Engine and Populating the Knowledge Base
----------------------------------------------------------

After writing the rules, we need to:

- define the rules to seresye
- instantiate the engine;
- populate the knowledge base with a set of initial facts.

We define the rules to SERESYE by defined a 'rules' attribute at the start of the module.

    %%%
    %%% relatives.erl
    %%%
    -module (relatives).

    -export([father/3, grandfather/3, grandmother/3,
             mother/3, brother/4, sister/4, start/0]).

    -rules([mother, father, brother, sister, grandfather,
            grandmother]).

We continue on to instantiate the engine and populate the knowledge base in the function *start* below:

    start () ->
      application:start(seresye) % Only if it is not already started
      seresye_srv:start(relatives),
      seresye_srv:add_rules(relatives, ?MODULE)

      seresye_srv:assert(relatives,
                     [{male, bob}, {male, corrado}, {male, mark}, {male, caesar},
                      {female, alice}, {female, sara}, {female, jane}, {female, anna},
                      {parent, jane, bob}, {parent, corrado, bob},
                      {parent, jane, mark}, {parent, corrado, mark},
                      {parent, jane, alice}, {parent, corrado, alice},
                      {parent, bob, caesar}, {parent, bob, anna},
                      {parent, sara, casear}, {parent, sara, anna}]),
      ok.

As the listing reports, creating a new SERESYE engine implies to call the function *seresye_srv:start/1*, giving the
name of the engine to be created

Then, we have to add the rules to the engine by using the function
*seresye_srv:add_rule/2*: it takes two arguments, the name of the engine and a tuple representing the function in the
form *{Module, FuncName}*; obviously the function *Module:FuncName* must be exported. Function *add_rule* has to be
called for each rule that has to be added; for this reason, the code above has an iteration over the list of rules
written before.

Finally, we populate the inference engine with a set of sample facts by giving them, in a list, to the function *
seresye:assert/2*. To test our rules, we considered the relationships in the Figure below and assert only the facts
for *male*, *female* and *parent*.

Testing the application
-----------------------

The final complete code of our AI application is thus the following:

    %%%
    %%% relatives.erl
    %%%
    -module (relatives).
    -export([father/3, grandfather/3, grandmother/3,
             mother/3, brother/4, sister/4, start/0]).

    -rules([mother, father, brother, sister, grandfather,
             grandmother]).

    %%
    %% if (X is female) and (X is Y's parent) then (X is Y's mother)
    %%
    mother(Engine, {female, X}, {parent, X, Y}) ->
      seresye_srv:assert(Engine, {mother, X, Y}).

    %%
    %% if (X is male) and (X is Y's parent) then (X is Y's father)
    %%
    father(Engine, {male, X}, {parent, X, Y}) ->
      seresye_srv:assert(Engine, {father, X, Y}).

    %%
    %% if (Y and Z have the same parent X) and (Z is female)
    %%    then (Z is Y's sister)
    %%
    sister(Engine, {parent, X, Y}, {parent, X, Z}, {female, Z}) when Y =/= Z ->
      seresye_srv:assert(Engine, {sister, Z, Y}).

    %%
    %% if (Y and Z have the same parent X) and (Z is male)
    %%    then (Z is Y's brother)
    %%
    brother(Engine, {parent, X, Y}, {parent, X, Z}, {male, Z}) when Y =/= Z ->
      seresye_srv:assert(Engine, {brother, Z, Y}).

    %%
    %% if (X is Y's father) and (Y is Z's parent)
    %%    then (X is Z's grandfather)
    %%
    grandfather (Engine, {father, X, Y}, {parent, Y, Z}) ->
      seresye_srv:assert (Engine, {grandfather, X, Z}).

    %%
    %% if (X is Y's mother) and (Y is Z's parent)
    %%    then (X is Z's grandmother)
    %%
    grandmother(Engine, {mother, X, Y}, {parent, Y, Z}) ->
      seresye_srv:assert(Engine, {grandmother, X, Z}).

    start () ->
      application:start(seresye_srv), % Only if it is not already started
      seresye_srv:start (relatives),
      seresye_srv:add_rules(relatives, ?MODULE)

      seresye_srv:assert (relatives,
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

Now it's time to test our application. 
Because we configured [rebar.config](https://github.com/MiloudEloumri/seresye/blob/master/rebar.config), 
to run the /examples/... on test profile,
We can run: *rebar3 as test shell* 
and start relatives engine using: *relatives:start().*
as follows:

    $ rebar3 as test shell
    ===> Verifying dependencies...
    ===> Fetching erlware_commons (from {git,"https://github.com/erlware/erlware_commons.git",
    {branch,"master"}})
    ===> Fetching parse_trans (from {git,"https://github.com/uwiger/parse_trans.git",
    {branch,"master"}})
    ===> Fetching cf v0.3.1
    ===> Analyzing applications...
    ===> Compiling cf
    ===> Compiling erlware_commons
    ===> Compiling parse_trans
    ===> Analyzing applications...
    ===> Compiling seresye
    ===> Booted cf
    ===> Booted erlware_commons
    ===> Booted seresye
    Eshell V11.0  (abort with ^G)
    1> relatives:start().
    ok
    2>


Following the call to function *relatives:start/0*, the engine is created and populated; if no errors occurred, the
rules should have been processed and the new facts derived. To check this, we can use the function *seresye_srv:
get_kb/1*, which returns the list of facts asserted into the knowledge base of a given engine:

    2> seresye_srv:get_kb(relatives).
    [{sister,anna,casear},
    {mother,sara,anna},
    {parent,sara,anna},
    {mother,sara,casear},
    {parent,sara,casear},
    {brother,caesar,anna},
    {sister,anna,caesar},
    {grandfather,corrado,anna},
    {grandmother,jane,anna},
    {father,bob,anna},
    {parent,bob,anna},
    {grandfather,corrado,caesar},
    {grandmother,jane,caesar},
    {father,bob,caesar},
    {parent,bob,caesar},
    {father,corrado,alice},
    {parent,corrado,alice},
    {brother,bob,alice},
    {brother,mark,alice},
    {sister,alice,bob},
    {sister,alice,mark},
    {mother,jane,alice},
    {parent,jane,alice},
    {father,corrado,mark},
    {parent,corrado,mark},
    {brother,mark,bob},
    {brother,bob,...},
    {mother,...},
    {...}|...]
    3>

The presence of facts representing concepts like *father*, *sister*, etc., proves that the rules seems to be working as
expected.

We can however query the knowledge base using specific fact templates. For example, if we want to know who are Alice's
brothers, we can use the function *seresye_srv:query_kb/2* as follows:

    3> seresye_srv:query_kb(relatives, {brother, '_', alice}).
    [{brother,bob,alice},{brother,mark,alice}]
    4>


The facts returned conform to the relationships depicted in the figure above, thus proving that the rules written are
really working.

As the example shows, function *seresye_srv:query_kb/2* takes the engine name as the first argument, while, for the
second parameter, a tuple has to be specified, representing the fact template to be matched; in such a tuple, the
atom *'_'* plays the role of a wildcard. However, to specify a more complex matching, a *fun* can be used as a tuple
element; this *fun* has to return a boolean value which indicates if the element matches the template. For example, to
select both Alice's and Anna's brothers, we can use the following function call:

    4> seresye_srv:query_kb(relatives, {brother, '_', fun (X) -> (X == alice) or (X == anna) end}).
    [{brother,caesar,anna},
    {brother,bob,alice},
    {brother,mark,alice}]
    5>

---
Running Examples and Eunit Tests
-----------

    $ rebar3 as test shell

Then try the examples and tests. For example:

    relatives:start().
    seresye_srv:get_kb(relatives).
    seresye_srv:query_kb(relatives, {brother, '_', alice}).
    seresye_srv:query_kb(relatives, {brother, '_', fun (X) -> (X == alice) or (X == anna) end}).

    seresye_phil:start().

    seresye_prodcons:start().

    seresyee_auto:start().


    eunit:test(seresyet_relatives).
    eunit:test(seresyet_cannibals).
    eunit:test(seresyet_simple_relatives).
    eunit:test(seresyet_sieve).

To run all Eunit tests use:

    $ rebar3 eunit

All Examples and Eunit tests are tested using,
*rebar 3.17.0 on Erlang/OTP 23 Erts 11.0*
---
Conclusions
-----------

This HowTo not only shows how to use the SERESYE engine to write an AI application, but also highlights the versatility
of the Erlang language:
the characteristics of functional and symbolic programming, together with the possibility of performing *introspection*
of function declaration, can be successfully exploited for application domains which are completely new for Erlang but
can surely be very interesting.