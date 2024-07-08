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
%%% SERESYE - Job Match App created by:
%%% Miloud Eloumri, <miloud.eloumri@gmail.com>,
%%% Compiled with rebar 3.20.0 on Erlang/OTP 23 Erts 11.0
%%% Uses:
%%% https://github.com/MiloudEloumri/seresye
%%% https://github.com/fogfish/semantic/tree/master
%%%
%%%-------------------------------------------------------------------
%%%
%%%-------------------------------------------------------------------
%%% @doc SERESYE - Job Match.
%%% @version 0.0.5.
%%% Updated : 25. November 2023 7:30 PM.
%%%
%%% @end
%%%-------------------------------------------------------------------
%%%
-module(job_match).

-export([match/3, start/0, edu_eval/5, exp_eval/6, skills_eval/8, score_eval/3,
         star_eval/1]).

-rules([match]).

%% Rule to match jobs and applicants
%% If there is a match based on a job ID in the two tuples argument (job facts tuple and applicant facts tuple),then,
%% a rule is fired, and, the actions associated with the rule are executed.
%% The actions are functions representing evolution of the matched applicant.
%% The return of any match rule is added to the KB representing an inferred fact.
match(Engine, {job, JobID, JobRest}, {applicant, JobID, ApplicantRest}) ->
    %% Extract job and applicant details
    {JName,
     {{education, JEduLevel, JEduField1, JEduField2},
      {experience, JExpYears, JExpField1, JExpField2},
      {techskills, JTechSkills1, JTechSkills2},
      {softskills, JSoftSkills1, JSoftSkills2}}} =
        JobRest,
    {ApplicantId,
     {ApplicantName,
      {education, ApplicantEduLevel, ApplicantEduField},
      {experience, ApplicantExpYears, ApplicantExpField1, ApplicantExpField2},
      {techskills, ApplicantTechSkills1, ApplicantTechSkills2},
      {softskills, ApplicantSoftSkills1, ApplicantSoftSkills2}}} =
        ApplicantRest,

    %% Evaluate criteria, normalize, calculate total score and star rating
    EduResult =
        edu_eval(JEduLevel, JEduField1, JEduField2, ApplicantEduLevel, ApplicantEduField),

    ExpResult =
        exp_eval(JExpYears,
                 JExpField1,
                 JExpField2,
                 ApplicantExpYears,
                 ApplicantExpField1,
                 ApplicantExpField2),

    SkillsResult =
        skills_eval(JTechSkills1,
                    JTechSkills2,
                    JSoftSkills1,
                    JSoftSkills2,
                    ApplicantTechSkills1,
                    ApplicantTechSkills2,
                    ApplicantSoftSkills1,
                    ApplicantSoftSkills2),
    
    {_, _, EduNormalized} = EduResult,
    {_, _, ExpNormalized} = ExpResult,
    {_, _, SkillsNormalized} = SkillsResult,

    TotalScore = score_eval(EduNormalized, ExpNormalized, SkillsNormalized),
    {score, Score} = TotalScore,

    StarRating = star_eval(Score),

    %% Build match fact result in a tuple structure 
    EvalResult =
        {match,
         JobID,
         ApplicantId,
         {JName, ApplicantName, TotalScore, StarRating, EduResult, ExpResult, SkillsResult}},

    %% Assert match fact
    %% This will add facts to the KB if rule processing pattern matches (a match rule pattern matches with existing initial KB facts)
    seresye_engine:set_client_state(
        seresye_engine:assert(Engine, EvalResult),
        [EvalResult | seresye_engine:get_client_state(Engine)]).

%% Education evaluation function
%% Implements the logic for education evaluation based on the proposed WSM formula
%% Returns education evaluation result in a tuple
edu_eval(JEduLevel, JEduField1, JEduField2, ApplicantEduLevel, ApplicantEduField) ->
    %% Evaluate based on WSM formula
    WeightJEduLevel = edu_level_weight(JEduLevel),
    WeightApplicantEduLevel = edu_level_weight(ApplicantEduLevel),
    WeightJEduField = 2,
    ApplicantEduFieldValue =
        if ApplicantEduField =:= JEduField1; ApplicantEduField =:= JEduField2 ->
               2;
           true ->
               1
        end,
    EducationPoints =
        WeightJEduLevel * WeightApplicantEduLevel + WeightJEduField * ApplicantEduFieldValue,
    MaxPoints = WeightJEduLevel * 4 + 4,
    MinPoints = 2,
    NormalizedEduPoints = (EducationPoints - MinPoints) / (MaxPoints - MinPoints) * 100,
    NormalizedEduPointsFormatted =
        list_to_float(io_lib:format("~.2f", [NormalizedEduPoints])),
    {education, EducationPoints, NormalizedEduPointsFormatted}.

edu_level_weight(EduLevel) ->
    case EduLevel of
        <<"Diploma">> ->
            1;
        <<"Bachelor">> ->
            2;
        <<"Master">> ->
            3;
        <<"Doctorate">> ->
            4;
        _ ->
            0
    end.

%% Experience evaluation function
%% Implements the logic for experience evaluation based on the proposed WSM formula
%% Returns experience evaluation result in a tuple
exp_eval(JExpYears,
         JExpField1,
         JExpField2,
         ApplicantExpYears,
         ApplicantExpField1,
         ApplicantExpField2) ->
    WeightJExpYears = exp_years_weight(JExpYears),
    if is_number(JExpYears), JExpYears >= 0, is_number(ApplicantExpYears),
       ApplicantExpYears >= 0 ->
           SumWeightedApplicantExpYears =
               sum_weighted_experience_years(JExpYears, ApplicantExpYears),
           WeightJExpField = 2,
           ApplicantExpFieldValue =
               if ApplicantExpField1 =:= JExpField1;
                  ApplicantExpField1 =:= JExpField2;
                  ApplicantExpField2 =:= JExpField1;
                  ApplicantExpField2 =:= JExpField2 ->
                      2;
                  true ->
                      1
               end,
           ExperiencePoints =
               WeightJExpYears * SumWeightedApplicantExpYears
               + WeightJExpField * ApplicantExpFieldValue,
           MaxPoints =
               case ExperiencePoints of
                   2 ->
                       4;
                   _ ->
                       WeightJExpYears * (JExpYears + (15 - JExpYears) * 1.5) + 4
               end;
       true ->
           ExperiencePoints = 2,
           MaxPoints = 4
    end,
    MinPoints = 2,
    NormalizedExpPoints = (ExperiencePoints - MinPoints) / (MaxPoints - MinPoints) * 100,
    NormalizedExpPointsFormatted =
        list_to_float(io_lib:format("~.2f", [NormalizedExpPoints])),
    {experience, ExperiencePoints, NormalizedExpPointsFormatted}.

sum_weighted_experience_years(JExpYears, ApplicantExpYears) ->
    BasicYears = min(JExpYears, ApplicantExpYears),
    ExtraYears = max(ApplicantExpYears - JExpYears, 0),
    BasicYears + ExtraYears * 1.5.

exp_years_weight(ExpYears) ->
    case ExpYears of
        0 ->
            0;
        1 ->
            1;
        ExpYears when ExpYears >= 2, ExpYears =< 5 ->
            2;
        ExpYears when ExpYears > 5 ->
            3;
        _ ->
            0
    end.

%% Skills evaluation function
%% Implements the logic for skills evaluation based on the proposed WSM formula
%% Returns skills evaluation result in a tuple
skills_eval(JTechSkills1,
            JTechSkills2,
            JSoftSkills1,
            JSoftSkills2,
            ApplicantTechSkills1,
            ApplicantTechSkills2,
            ApplicantSoftSkills1,
            ApplicantSoftSkills2) ->
    % Technical Skills Match
    TechPoints =
        skill_match(JTechSkills1, ApplicantTechSkills1, ApplicantTechSkills2)
        + skill_match(JTechSkills2, ApplicantTechSkills1, ApplicantTechSkills2),
    % Soft Skills Match
    SoftPoints =
        skill_match(JSoftSkills1, ApplicantSoftSkills1, ApplicantSoftSkills2)
        + skill_match(JSoftSkills2, ApplicantSoftSkills1, ApplicantSoftSkills2),
    % Weights for Technical and Soft Skills
    WT = 0.6,
    WS = 0.4,
    % Evaluate Skills Points
    SkillsPoints = WT * TechPoints + WS * SoftPoints,
    % Normalization
    NormalizedSkillsPoints = SkillsPoints / (WT * 2 + WS * 2) * 100,
    NormalizedSkillsPointsFormatted =
        list_to_float(io_lib:format("~.2f", [NormalizedSkillsPoints])),
    {skills, SkillsPoints, NormalizedSkillsPointsFormatted}.

skill_match(JobSkill, ApplicantSkill1, ApplicantSkill2) ->
    case JobSkill of
        ApplicantSkill1 ->
            1;
        ApplicantSkill2 ->
            1;
        _ ->
            0
    end.

%% Total score evaluation function
%% Implements the logic for total score as the arithmetic mean of normalized values
%% Returns total score evaluation result in a tuple
score_eval(EduNormalized, ExpNormalized, SkillsNormalized) ->
    %% Evaluate total score as the arithmetic mean of normalized values
    Score = (EduNormalized + ExpNormalized + SkillsNormalized) / 3,
    ScoreFormatted = list_to_float(io_lib:format("~.2f", [Score])),
    %% Return total score evaluation results in a tuple
    {score, ScoreFormatted}.

%% Star Rating evaluation function
%% Implements the logic for star rating based on a maximum total score of %100.
%% Returns star rating evaluation result in a tuple
star_eval(Score) ->
    %% 0.5 star: 0-10%
    %% 1 star: >10-20%
    %% 1.5 stars: >20-30%
    %% 2 stars: >30-40%
    %% 2.5 stars: >40-50%
    %% 3 stars: >50-60%
    %% 3.5 stars: 60-70%
    %% 4 stars: 70-80%
    %% 4.5 stars: 80-90%
    %% 5 stars: 90-100%
    StarRating =
        case Score of
            _ when Score =< 10 ->
                0.5;
            _ when Score =< 20 ->
                1.0;
            _ when Score =< 30 ->
                1.5;
            _ when Score =< 40 ->
                2.0;
            _ when Score =< 50 ->
                2.5;
            _ when Score =< 60 ->
                3.0;
            _ when Score =< 70 ->
                3.5;
            _ when Score =< 80 ->
                4.0;
            _ when Score =< 90 ->
                4.5;
            _ ->
                5.0
        end,
    %% Return star rating evaluation result in a tuple
    {stars, StarRating}.

%% @doc Starts the semantic library, loads a data file, starts the seresye server, adds rules,
%% extracts triples from the data file, and asserts them in the seresye server.
%% @end
-spec start() -> ok | error.
start() ->
    % Start the semantic library. If it starts successfully, continue; otherwise, print an error message.
    case semantic:start() of
        % The semantic module started successfully.
        {ok, []} ->
            % Join the given path elements into a path to a data file.
            NTriples =
                filename:join([code:priv_dir(job_match),
                               "data",
                               "3-btm-jobs-applicants-ontology.nt"]),
            % Load the data file into a list.
            NTriplesStream =
                stream:list(
                    semantic:nt(NTriples)),
            % Type the loaded data (convert to a list of maps).
            NTriplesStreamTyped = semantic:typed(NTriplesStream),

            % Write streams to a txt file to check the patterns
            % This will write the file in the root of match app where rebar3 shell is run
            {ok, StreamFile} =
                file:open("4-btm-jobs-applicants-ontology-stream.txt",
                          [write]), %% A list of iri tuples
            {ok, StreamTypedFile} =
                file:open("5-btm-jobs-applicants-ontology-stream-typed.txt",
                          [write]), %% A list of maps
            io:format(StreamFile, "~p~n", [NTriplesStream]),
            io:format(StreamTypedFile, "~p~n", [NTriplesStreamTyped]),
            file:close(StreamFile),
            file:close(StreamTypedFile),

            % Start the seresye server with a given name.
            seresye_srv:start(job_match),
            % Add rules from the current module to the seresye server.
            seresye_srv:add_rules(job_match, ?MODULE),

            % Extract jobs data (facts) tuples from the typed data (list of maps).
            JobsListOfTuplesFacts = jobs_facts:extract_jobs_tuples(NTriplesStreamTyped),

            % Write jobs facts to a txt file for examination purposes
            % This will write the file in the root of match app where rebar3 shell is run
            {ok, JobsFacts} = file:open("6-jobs-asserted-facts-tuples.txt", [write]),
            io:format(JobsFacts, "~p~n", [JobsListOfTuplesFacts]),
            file:close(JobsFacts),

            % Extract applicants data (facts) tuples from the typed data (list of maps).
            ApplicantsListOfTuplesFacts =
                applicants_facts:extract_applicants_tuples(NTriplesStreamTyped),

            % Write applicants facts to a txt file for examination purposes
            % This will write the file in the root of match app where rebar3 shell is run
            {ok, ApplicantsFacts} = file:open("7-applicants-asserted-facts-tuples.txt", [write]),
            io:format(ApplicantsFacts, "~p~n", [ApplicantsListOfTuplesFacts]),
            file:close(ApplicantsFacts),

            %% Combine jobs facts and applicants facts in one list of tuples representing initial asserted KB
            JobsApplicantsListOfTuplesFacts = JobsListOfTuplesFacts ++ ApplicantsListOfTuplesFacts,

            % Write jobs and applicants facts (initial asserted KB) to a txt file for examination purposes
            % This will write the file in the root of match app where rebar3 shell is run
            {ok, JobsApplicantsFacts} =
                file:open("8-jobs-applicants-asserted-facts-kb.txt", [write]),
            io:format(JobsApplicantsFacts, "~p~n", [JobsApplicantsListOfTuplesFacts]),
            file:close(JobsApplicantsFacts),

            % Assert each tuple in jobs and applicants facts in the job_match engine.
            lists:foreach(fun(Fact) -> seresye_srv:assert(job_match, Fact) end,
                          JobsApplicantsListOfTuplesFacts),
            % The resulting KB holds all facts in a list of tuples including the facts resulted from firing matching rules
            % Write the asserted and inferred KB to a txt file for examination purposes
            % This will write the file in the root of match app where rebar3 shell is run
            KB = seresye_srv:get_kb(job_match),
            {ok, KBFile} = file:open("9-jobs-applicants-asserted-inferred-kb.txt", [write]),
            io:format(KBFile, "~p~n", [KB]),
            file:close(KBFile),
            ok;
        % The semantic module did not start successfully. Print an error message.
        {error, Reason} ->
            io:format("Error starting semantic: ~p~n", [Reason]),
            error
    end.
