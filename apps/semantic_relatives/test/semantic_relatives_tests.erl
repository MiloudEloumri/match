-module(semantic_relatives_tests).

-include_lib("eunit/include/eunit.hrl").

rules_test() ->
    Engine0 = seresye_engine:new([]),
    Engine2 = seresye_engine:add_rules(Engine0, semantic_relatives),

    Engine3 =
        seresye_engine:assert(Engine2,
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

    InternalState = seresye_engine:get_client_state(Engine3),

    ?assertMatch(true, lists:member({mother, sara, anna}, InternalState)),

    ?assertMatch(true, lists:member({sister, anna, casear}, InternalState)),

    ?assertMatch(true, lists:member({mother, sara, casear}, InternalState)),

    ?assertMatch(true, lists:member({grandfather, corrado, anna}, InternalState)),

    ?assertMatch(true, lists:member({sister, anna, caesar}, InternalState)),

    ?assertMatch(true, lists:member({brother, caesar, anna}, InternalState)),

    ?assertMatch(true, lists:member({father, bob, anna}, InternalState)),

    ?assertMatch(true, lists:member({grandmother, jane, anna}, InternalState)),

    ?assertMatch(true, lists:member({grandfather, corrado, caesar}, InternalState)),

    ?assertMatch(true, lists:member({father, bob, caesar}, InternalState)),

    ?assertMatch(true, lists:member({grandmother, jane, caesar}, InternalState)),

    ?assertMatch(true, lists:member({sister, alice, mark}, InternalState)),

    ?assertMatch(true, lists:member({brother, bob, alice}, InternalState)),

    ?assertMatch(true, lists:member({brother, mark, alice}, InternalState)),

    ?assertMatch(true, lists:member({father, corrado, alice}, InternalState)),

    ?assertMatch(true, lists:member({sister, alice, bob}, InternalState)),

    ?assertMatch(true, lists:member({sister, alice, mark}, InternalState)),

    ?assertMatch(true, lists:member({brother, bob, alice}, InternalState)),

    ?assertMatch(true, lists:member({brother, mark, alice}, InternalState)),

    ?assertMatch(true, lists:member({mother, jane, alice}, InternalState)),

    ?assertMatch(true, lists:member({sister, alice, bob}, InternalState)),

    ?assertMatch(true, lists:member({brother, bob, mark}, InternalState)),

    ?assertMatch(true, lists:member({father, corrado, mark}, InternalState)),

    ?assertMatch(true, lists:member({brother, mark, bob}, InternalState)),

    ?assertMatch(true, lists:member({brother, bob, mark}, InternalState)),

    ?assertMatch(true, lists:member({mother, jane, mark}, InternalState)),

    ?assertMatch(true, lists:member({brother, mark, bob}, InternalState)),

    ?assertMatch(true, lists:member({father, corrado, bob}, InternalState)),

    ?assertMatch(true, lists:member({mother, jane, bob}, InternalState)),

    ?assertMatch(29, erlang:length(InternalState)).

semantic_relatives_test_() ->
    [?_test(rules_test())].
