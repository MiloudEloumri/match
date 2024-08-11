%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for the job_match module using EUnit.
%%% This module contains tests to validate the job matching logic.
%%%
%%% To run these tests, use the following command:
%%% rebar3 eunit --module=job_match_tests
%%%
%%% This will compile the necessary modules and execute the tests
%%% defined in this module.
%%%
%%% The tests are designed to:
%%% - Set up the initial state with rules and sample data.
%%% - Verify the matching logic for different applicants.
%%%
%%% Each test function is designed to test specific parts of the
%%% matching logic and ensure the correct behavior.
%%%-------------------------------------------------------------------

-module(job_match_tests).

-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% Sets up the initial engine state by creating a new seresye engine
%%% and adding the rules defined in the job_match module.
%%%
%%% @return Engine2 The engine state with the added rules.
%%%-------------------------------------------------------------------
setup() ->
    Engine0 = seresye_engine:new([]),
    seresye_engine:add_rules(Engine0, job_match).

%%%-------------------------------------------------------------------
%%% @doc
%%% Provides a list of sample applicants and job data to be used
%%% in the tests. Each tuple represents a job or applicant fact.
%%%
%%% @return List of tuples representing job and applicant facts.
%%%-------------------------------------------------------------------
test_applicants() ->
    [{job,
      <<"JPosting1">>,
      {<<"DigitalSecurityManagerOfficer">>,
       {{education, <<"Master">>, <<"ComputerScience">>, <<"Informationtechnology">>},
        {experience, 10, <<"InformationTechnologyExperience">>, <<"BusinessTechnology">>},
        {techskills, <<"ComplexProblemSolving">>, <<"JudgmentAndDecisionMaking">>},
        {softskills, <<"DecisionMaking">>, <<"OralCommunication">>}}}},
     {applicant,
      <<"JPosting1">>,
      {<<"JSeeker1">>,
       {<<"AustinLeigh">>,
        {education, <<"Diploma">>, <<"ComputerScience">>},
        {experience,
         10,
         <<"InformationTechnologyExperience">>,
         <<"BusinessAdministrationExperiance">>},
        {techskills, <<"ComplexProblemSolving">>, <<"JudgmentAndDecisionMaking">>},
        {softskills, <<"DecisionMaking">>, <<"OralCommunication">>}}}},
     {applicant,
      <<"JPosting1">>,
      {<<"JSeeker2">>,
       {<<"AshwinMohammed">>,
        {education, <<"Diploma">>, <<"InformationTechnology">>},
        {experience, 7, <<"InformationTechnologyExperience">>, <<"Nil">>},
        {techskills, <<"ComplexProblemSolving">>, <<"InformationAnalysisTechniques">>},
        {softskills, <<"FindingInformation">>, <<"Cooperation">>}}}},
     {applicant,
      <<"JPosting1">>,
      {<<"JSeeker3">>,
       {<<"SteffanBeattie">>,
        {education, <<"Bachelor">>, <<"ComputerScience">>},
        {experience, 6, <<"ProcessImprovement">>, <<"TestingSpecialist">>},
        {techskills, <<"RiskIssueManagement">>, <<"ConductingInDepthAnalysis">>},
        {softskills, <<"DecisionMaking">>, <<"PresentationAndPublicSpeakingSkills">>}}}}].

%%%-------------------------------------------------------------------
%%% @doc
%%% Tests the overall job matching logic by asserting the sample data
%%% and verifying the matches for multiple applicants.
%%% This function performs the following steps:
%%% - Sets up the engine state.
%%% - Asserts the job and applicant facts.
%%% - Retrieves the internal state of the engine.
%%% - Validates the matches and scores for each applicant.
%%%
%%% @return none
%%%-------------------------------------------------------------------
rules_test() ->
    Engine2 = setup(),
    Applicants = test_applicants(),
    Engine3 = seresye_engine:assert(Engine2, Applicants),
    InternalState = seresye_engine:get_client_state(Engine3),

    io:format("Internal State: ~p~n", [InternalState]),

    ?assertMatch(true,
                 lists:member([{match,
                                <<"JPosting1">>,
                                <<"JSeeker1">>,
                                {<<"DigitalSecurityManagerOfficer">>,
                                 <<"AustinLeigh">>,
                                 {score, 64.81},
                                 {stars, 3.5},
                                 {education, 7, 35.71},
                                 {experience, 34.0, 58.72},
                                 {skills, 2.0, 100.0}}},
                               {strict,
                                <<"JPosting1">>,
                                <<"JSeeker1">>,
                                {<<"DigitalSecurityManagerOfficer">>,
                                 <<"AustinLeigh">>,
                                 {score, 27.41},
                                 {stars, 1.5},
                                 {education, 4, 14.29},
                                 {experience, 30.0, 51.38},
                                 {skills, 1.0, 50.0}}}],
                              InternalState)),

    ?assertMatch(true,
                 lists:member([{match,
                                <<"JPosting1">>,
                                <<"JSeeker2">>,
                                {<<"DigitalSecurityManagerOfficer">>,
                                 <<"AshwinMohammed">>,
                                 {score, 31.21},
                                 {stars, 2.0},
                                 {education, 5, 21.43},
                                 {experience, 25.0, 42.2},
                                 {skills, 0.6, 30.0}}},
                               {strict,
                                <<"JPosting1">>,
                                <<"JSeeker2">>,
                                {<<"DigitalSecurityManagerOfficer">>,
                                 <<"AshwinMohammed">>,
                                 {score, 0.0},
                                 {stars, 0.0},
                                 {education, 0, 0.0},
                                 {experience, 0.0, 0.0},
                                 {skills, 0.0, 0.0}}}],
                              InternalState)),

    ?assertMatch(true,
                 lists:member([{match,
                                <<"JPosting1">>,
                                <<"JSeeker3">>,
                                {<<"DigitalSecurityManagerOfficer">>,
                                 <<"SteffanBeattie">>,
                                 {score, 36.72},
                                 {stars, 2.0},
                                 {education, 10, 57.14},
                                 {experience, 20.0, 33.03},
                                 {skills, 0.4, 20.0}}},
                               {strict,
                                <<"JPosting1">>,
                                <<"JSeeker3">>,
                                {<<"DigitalSecurityManagerOfficer">>,
                                 <<"SteffanBeattie">>,
                                 {score, 0.0},
                                 {stars, 0.0},
                                 {education, 4, 14.29},
                                 {experience, 0.0, 0.0},
                                 {skills, 0.0, 0.0}}}],
                              InternalState)),

    ?assertMatch(3, erlang:length(InternalState)).

%%%-------------------------------------------------------------------
%%% @doc
%%% Tests the matching logic for the first applicant (JSeeker1).
%%% This function performs the following steps:
%%% - Sets up the engine state.
%%% - Asserts the job and applicant facts.
%%% - Retrieves the internal state of the engine.
%%% - Validates the match and score for the first applicant.
%%%
%%% @return none
%%%-------------------------------------------------------------------
test_match_seeker1() ->
    Engine2 = setup(),
    Applicants = test_applicants(),
    Engine3 = seresye_engine:assert(Engine2, Applicants),
    InternalState = seresye_engine:get_client_state(Engine3),

    ?assertMatch(true,
                 lists:member([{match,
                                <<"JPosting1">>,
                                <<"JSeeker1">>,
                                {<<"DigitalSecurityManagerOfficer">>,
                                 <<"AustinLeigh">>,
                                 {score, 64.81},
                                 {stars, 3.5},
                                 {education, 7, 35.71},
                                 {experience, 34.0, 58.72},
                                 {skills, 2.0, 100.0}}},
                               {strict,
                                <<"JPosting1">>,
                                <<"JSeeker1">>,
                                {<<"DigitalSecurityManagerOfficer">>,
                                 <<"AustinLeigh">>,
                                 {score, 27.41},
                                 {stars, 1.5},
                                 {education, 4, 14.29},
                                 {experience, 30.0, 51.38},
                                 {skills, 1.0, 50.0}}}],
                              InternalState)).

%%%-------------------------------------------------------------------
%%% @doc
%%% Tests the matching logic for the second applicant (JSeeker2).
%%% This function performs the following steps:
%%% - Sets up the engine state.
%%% - Asserts the job and applicant facts.
%%% - Retrieves the internal state of the engine.
%%% - Validates the match and score for the second applicant.
%%%
%%% @return none
%%%-------------------------------------------------------------------
test_match_seeker2() ->
    Engine2 = setup(),
    Applicants = test_applicants(),
    Engine3 = seresye_engine:assert(Engine2, Applicants),
    InternalState = seresye_engine:get_client_state(Engine3),

    ?assertMatch(true,
                 lists:member([{match,
                                <<"JPosting1">>,
                                <<"JSeeker2">>,
                                {<<"DigitalSecurityManagerOfficer">>,
                                 <<"AshwinMohammed">>,
                                 {score, 31.21},
                                 {stars, 2.0},
                                 {education, 5, 21.43},
                                 {experience, 25.0, 42.2},
                                 {skills, 0.6, 30.0}}},
                               {strict,
                                <<"JPosting1">>,
                                <<"JSeeker2">>,
                                {<<"DigitalSecurityManagerOfficer">>,
                                 <<"AshwinMohammed">>,
                                 {score, 0.0},
                                 {stars, 0.0},
                                 {education, 0, 0.0},
                                 {experience, 0.0, 0.0},
                                 {skills, 0.0, 0.0}}}],
                              InternalState)).

%%%-------------------------------------------------------------------
%%% @doc
%%% Tests the matching logic for the third applicant (JSeeker3).
%%% This function performs the following steps:
%%% - Sets up the engine state.
%%% - Asserts the job and applicant facts.
%%% - Retrieves the internal state of the engine.
%%% - Validates the match and score for the third applicant.
%%%
%%% @return none
%%%-------------------------------------------------------------------
test_match_seeker3() ->
    Engine2 = setup(),
    Applicants = test_applicants(),
    Engine3 = seresye_engine:assert(Engine2, Applicants),
    InternalState = seresye_engine:get_client_state(Engine3),

    ?assertMatch(true,
                 lists:member([{match,
                                <<"JPosting1">>,
                                <<"JSeeker3">>,
                                {<<"DigitalSecurityManagerOfficer">>,
                                 <<"SteffanBeattie">>,
                                 {score, 36.72},
                                 {stars, 2.0},
                                 {education, 10, 57.14},
                                 {experience, 20.0, 33.03},
                                 {skills, 0.4, 20.0}}},
                               {strict,
                                <<"JPosting1">>,
                                <<"JSeeker3">>,
                                {<<"DigitalSecurityManagerOfficer">>,
                                 <<"SteffanBeattie">>,
                                 {score, 0.0},
                                 {stars, 0.0},
                                 {education, 4, 14.29},
                                 {experience, 0.0, 0.0},
                                 {skills, 0.0, 0.0}}}],
                              InternalState)).

%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit test generator function. This function collects all individual
%%% test functions and runs them as part of the EUnit test suite.
%%%
%%% @return List of tests to be executed.
%%%-------------------------------------------------------------------
job_match_test_() ->
    [?_test(test_match_seeker1()),
     ?_test(test_match_seeker2()),
     ?_test(test_match_seeker3()),
     ?_test(rules_test())].
