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

-include("job_match_mnesia_kb.hrl").

-export([match/3, start/0, edu_eval/6, exp_eval/7, skills_eval/9, score_eval/4,
         star_eval/1]).

-rules([match]).

%% Rule to match jobs and applicants
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
    %% Validate extracted data
    case is_valid_data({JName,
                        {JEduLevel, JEduField1, JEduField2},
                        {JExpYears, JExpField1, JExpField2},
                        {JTechSkills1, JTechSkills2},
                        {JSoftSkills1, JSoftSkills2}},
                       {ApplicantId,
                        {ApplicantName,
                         ApplicantEduLevel,
                         ApplicantEduField,
                         ApplicantExpYears,
                         ApplicantExpField1,
                         ApplicantExpField2,
                         ApplicantTechSkills1,
                         ApplicantTechSkills2,
                         ApplicantSoftSkills1,
                         ApplicantSoftSkills2}})
    of
        true ->
            %% Evaluate criteria, normalize, calculate total score and star rating
            %% Default Mode
            EduResultDefault =
                edu_eval(default,
                         JEduLevel,
                         JEduField1,
                         JEduField2,
                         ApplicantEduLevel,
                         ApplicantEduField),
            ExpResultDefault =
                exp_eval(default,
                         JExpYears,
                         JExpField1,
                         JExpField2,
                         ApplicantExpYears,
                         ApplicantExpField1,
                         ApplicantExpField2),
            SkillsResultDefault =
                skills_eval(default,
                            JTechSkills1,
                            JTechSkills2,
                            JSoftSkills1,
                            JSoftSkills2,
                            ApplicantTechSkills1,
                            ApplicantTechSkills2,
                            ApplicantSoftSkills1,
                            ApplicantSoftSkills2),
            {_, _, EduNormalizedDefault} = EduResultDefault,
            {_, _, ExpNormalizedDefault} = ExpResultDefault,
            {_, _, SkillsNormalizedDefault} = SkillsResultDefault,
            TotalScoreDefault =
                score_eval(default,
                           EduNormalizedDefault,
                           ExpNormalizedDefault,
                           SkillsNormalizedDefault),
            {score, ScoreDefault} = TotalScoreDefault,
            StarRatingDefault = star_eval(ScoreDefault),
            %% Build default mode match fact result in a tuple structure
            EvalResultDefault =
                {match,
                 JobID,
                 ApplicantId,
                 {JName,
                  ApplicantName,
                  TotalScoreDefault,
                  StarRatingDefault,
                  EduResultDefault,
                  ExpResultDefault,
                  SkillsResultDefault}},
            %% Strict Mode
            EduResultStrict =
                edu_eval(strict,
                         JEduLevel,
                         JEduField1,
                         JEduField2,
                         ApplicantEduLevel,
                         ApplicantEduField),
            ExpResultStrict =
                exp_eval(strict,
                         JExpYears,
                         JExpField1,
                         JExpField2,
                         ApplicantExpYears,
                         ApplicantExpField1,
                         ApplicantExpField2),
            SkillsResultStrict =
                skills_eval(strict,
                            JTechSkills1,
                            JTechSkills2,
                            JSoftSkills1,
                            JSoftSkills2,
                            ApplicantTechSkills1,
                            ApplicantTechSkills2,
                            ApplicantSoftSkills1,
                            ApplicantSoftSkills2),
            {_, _, EduNormalizedStrict} = EduResultStrict,
            {_, _, ExpNormalizedStrict} = ExpResultStrict,
            {_, _, SkillsNormalizedStrict} = SkillsResultStrict,
            TotalScoreStrict =
                score_eval(strict,
                           EduNormalizedStrict,
                           ExpNormalizedStrict,
                           SkillsNormalizedStrict),
            {score, ScoreStrict} = TotalScoreStrict,
            StarRatingStrict = star_eval(ScoreStrict),
            %% Build strict match fact result in a tuple structure
            EvalResultStrict =
                {strict,
                 JobID,
                 ApplicantId,
                 {JName,
                  ApplicantName,
                  TotalScoreStrict,
                  StarRatingStrict,
                  EduResultStrict,
                  ExpResultStrict,
                  SkillsResultStrict}},
            %% Combine default and strict match facts results in a tuple structure
            CombinedEvalResult = [EvalResultDefault, EvalResultStrict],
            %% Flatten the combined result list
            %% FlattenedEvalResult = lists:flatten(CombinedEvalResult),
            %% Assert default and strict match fact
            seresye_engine:set_client_state(
                seresye_engine:assert(Engine, CombinedEvalResult),
                [CombinedEvalResult | seresye_engine:get_client_state(Engine)]);
        false ->
            %% Invalid data, do nothing
            ok
    end.

%% Function to validate the data
is_valid_data({JName,
               {JEduLevel, JEduField1, JEduField2},
               {JExpYears, JExpField1, JExpField2},
               {JTechSkills1, JTechSkills2},
               {JSoftSkills1, JSoftSkills2}},
              {ApplicantId,
               {ApplicantName,
                ApplicantEduLevel,
                ApplicantEduField,
                ApplicantExpYears,
                ApplicantExpField1,
                ApplicantExpField2,
                ApplicantTechSkills1,
                ApplicantTechSkills2,
                ApplicantSoftSkills1,
                ApplicantSoftSkills2}}) ->
    JName =/= undefined
    andalso JEduLevel =/= undefined
    andalso JEduField1 =/= undefined
    andalso JEduField2 =/= undefined
    andalso JExpYears =/= undefined
    andalso JExpField1 =/= undefined
    andalso JExpField2 =/= undefined
    andalso JTechSkills1 =/= undefined
    andalso JTechSkills2 =/= undefined
    andalso JSoftSkills1 =/= undefined
    andalso JSoftSkills2 =/= undefined
    andalso ApplicantId =/= undefined
    andalso ApplicantName =/= undefined
    andalso ApplicantEduLevel =/= undefined
    andalso ApplicantEduField =/= undefined
    andalso ApplicantExpYears =/= undefined
    andalso ApplicantExpField1 =/= undefined
    andalso ApplicantExpField2 =/= undefined
    andalso ApplicantTechSkills1 =/= undefined
    andalso ApplicantTechSkills2 =/= undefined
    andalso ApplicantSoftSkills1 =/= undefined
    andalso ApplicantSoftSkills2 =/= undefined.

%% Education evaluation function
edu_eval(default,
         JEduLevel,
         JEduField1,
         JEduField2,
         ApplicantEduLevel,
         ApplicantEduField) ->
    %% Calculate weights
    WeightJEduLevel = edu_level_weight(JEduLevel),
    WeightApplicantEduLevel = edu_level_weight(ApplicantEduLevel),
    WeightJEduField = 2,
    %% Determine field match value
    ApplicantEduFieldValue =
        if ApplicantEduField =:= JEduField1; ApplicantEduField =:= JEduField2 ->
               2;
           true ->
               1
        end,
    %% Calculate education points
    EducationPoints =
        WeightJEduLevel * WeightApplicantEduLevel + WeightJEduField * ApplicantEduFieldValue,
    %% Calculate maximum possible points
    MaxPoints = WeightJEduLevel * 4 + WeightJEduField * 2,
    %% Determine minimum points
    MinPoints =
        case EducationPoints of
            0 ->
                0;
            _ ->
                2
        end,
    %% Normalize the education points
    NormalizedEduPoints = (EducationPoints - MinPoints) / (MaxPoints - MinPoints) * 100,
    NormalizedEduPointsFormatted =
        list_to_float(io_lib:format("~.2f", [NormalizedEduPoints])),
    {education, EducationPoints, NormalizedEduPointsFormatted};
edu_eval(strict,
         JEduLevel,
         JEduField1,
         JEduField2,
         ApplicantEduLevel,
         ApplicantEduField) ->
    %% Calculate weights
    WeightJEduLevel = edu_level_weight(JEduLevel),
    WeightApplicantEduLevel = edu_level_weight(ApplicantEduLevel),
    WeightJEduField = 2,
    %% Determine field match value
    ApplicantEduFieldValue =
        if ApplicantEduField =:= JEduField1; ApplicantEduField =:= JEduField2 ->
               2;
           true ->
               0
        end,
    %% Calculate education points
    EducationPoints =
        exact_match(JEduLevel, ApplicantEduLevel) * WeightJEduLevel * WeightApplicantEduLevel
        + exact_match_edu_fields(JEduField1, JEduField2, ApplicantEduField)
          * WeightJEduField
          * ApplicantEduFieldValue,
    %% Calculate maximum possible points
    MaxPoints = WeightJEduLevel * 4 + WeightJEduField * 2,
    %% Determine minimum points
    MinPoints =
        if EducationPoints =< 0 ->
               0;
           EducationPoints < 2 ->
               EducationPoints;
           true ->
               2
        end,
    %% Normalize the education points
    NormalizedEduPoints = (EducationPoints - MinPoints) / (MaxPoints - MinPoints) * 100,
    NormalizedEduPointsFormatted =
        list_to_float(io_lib:format("~.2f", [NormalizedEduPoints])),
    {education, EducationPoints, NormalizedEduPointsFormatted}.

exact_match(Value1, Value2) ->
    if Value1 =:= Value2 ->
           1;
       true ->
           0
    end.

exact_match_edu_fields(JEduField1, JEduField2, ApplicantEduField) ->
    if JEduField1 =:= ApplicantEduField; JEduField2 =:= ApplicantEduField ->
           1;
       true ->
           0
    end.

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
exp_eval(default,
         JExpYears,
         JExpField1,
         JExpField2,
         ApplicantExpYears,
         ApplicantExpField1,
         ApplicantExpField2) ->
    %% Weight calculation for job experience years
    WeightJExpYears = exp_years_weight(JExpYears),
    %% Check if input values are valid
    case {is_number(JExpYears),
          JExpYears >= 0,
          is_number(ApplicantExpYears),
          ApplicantExpYears >= 0}
    of
        {true, true, true, true} ->
            %% Calculate weighted sum of experience years
            SumWeightedApplicantExpYears =
                sum_weighted_experience_years(JExpYears, ApplicantExpYears),
            %% Determine the value for experience fields matching
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
            %% Calculate the total experience points
            ExperiencePoints =
                WeightJExpYears * SumWeightedApplicantExpYears
                + WeightJExpField * ApplicantExpFieldValue,
            %% Calculate the maximum possible points
            MaxPoints =
                if ExperiencePoints =< 2 ->
                       4;
                   true ->
                       WeightJExpYears * (min(JExpYears, 15) + (15 - min(JExpYears, 15)) * 1.5) + 4
                end;
        _ -> %% Handle invalid inputs
            ExperiencePoints = 2,
            MaxPoints = 4
    end,
    %% Determine minimum points
    MinPoints =
        if ExperiencePoints =< 0 ->
               0;
           ExperiencePoints < 2 ->
               ExperiencePoints;
           true ->
               2
        end,
    %% Normalize the experience points
    NormalizedExpPoints = (ExperiencePoints - MinPoints) / (MaxPoints - MinPoints) * 100,
    NormalizedExpPointsFormatted =
        list_to_float(io_lib:format("~.2f", [NormalizedExpPoints])),
    {experience, ExperiencePoints, NormalizedExpPointsFormatted};
exp_eval(strict,
         JExpYears,
         JExpField1,
         JExpField2,
         ApplicantExpYears,
         ApplicantExpField1,
         ApplicantExpField2) ->
    WeightJExpYears = exp_years_weight(JExpYears),
    %% Validate inputs
    case {is_number(JExpYears),
          JExpYears >= 0,
          is_number(ApplicantExpYears),
          ApplicantExpYears >= 0}
    of
        {true, true, true, true} ->
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
                       0
                end,
            ExperiencePoints =
                exact_match(JExpYears, ApplicantExpYears)
                * WeightJExpYears
                * SumWeightedApplicantExpYears
                + exact_match_exp_fields(JExpField1,
                                         JExpField2,
                                         ApplicantExpField1,
                                         ApplicantExpField2)
                  * WeightJExpField
                  * ApplicantExpFieldValue,
            %% Calculate MaxPoints
            MaxPoints =
                if ExperiencePoints =< 2 ->
                       2 * 2;
                   true ->
                       WeightJExpYears * (min(JExpYears, 15) + (15 - min(JExpYears, 15)) * 1.5)
                       + 2 * 2
                end,
            %% Determine MinPoints
            MinPoints =
                if ExperiencePoints =< 0 ->
                       0;
                   ExperiencePoints < 2 ->
                       ExperiencePoints;
                   true ->
                       2
                end;
        _ ->
            %% Handle invalid inputs
            ExperiencePoints = 0,
            MaxPoints = 2 * 2,
            MinPoints = 0
    end,
    %% Normalize experience points
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

exact_match_exp_fields(JExpField1, JExpField2, ApplicantExpField1, ApplicantExpField2) ->
    if (ApplicantExpField1 =:= JExpField1 orelse ApplicantExpField1 =:= JExpField2)
       andalso (ApplicantExpField2 =:= JExpField1 orelse ApplicantExpField2 =:= JExpField2) ->
           1;
       true ->
           0
    end.

%% Skills evaluation function
skills_eval(default,
            JTechSkills1,
            JTechSkills2,
            JSoftSkills1,
            JSoftSkills2,
            ApplicantTechSkills1,
            ApplicantTechSkills2,
            ApplicantSoftSkills1,
            ApplicantSoftSkills2) ->
    % Calculate technical skills points
    TechPoints =
        calculate_skill_points(JTechSkills1,
                               JTechSkills2,
                               ApplicantTechSkills1,
                               ApplicantTechSkills2),
    % Calculate soft skills points
    SoftPoints =
        calculate_skill_points(JSoftSkills1,
                               JSoftSkills2,
                               ApplicantSoftSkills1,
                               ApplicantSoftSkills2),
    % Weights for technical and soft skills
    WT = 0.6,
    WS = 0.4,
    % Evaluate total skills points
    SkillsPoints = WT * TechPoints + WS * SoftPoints,
    % Normalize the skills points
    NormalizedSkillsPoints = normalize_skills_points(SkillsPoints, WT, WS),
    NormalizedSkillsPointsFormatted =
        list_to_float(io_lib:format("~.2f", [NormalizedSkillsPoints])),
    {skills, SkillsPoints, NormalizedSkillsPointsFormatted};
skills_eval(strict,
            JTechSkills1,
            JTechSkills2,
            JSoftSkills1,
            JSoftSkills2,
            ApplicantTechSkills1,
            ApplicantTechSkills2,
            ApplicantSoftSkills1,
            ApplicantSoftSkills2) ->
    % Calculate technical skills points using strict matching
    TechPoints =
        calculate_strict_skill_points(JTechSkills1,
                                      JTechSkills2,
                                      ApplicantTechSkills1,
                                      ApplicantTechSkills2),
    % Calculate soft skills points using strict matching
    SoftPoints =
        calculate_strict_skill_points(JSoftSkills1,
                                      JSoftSkills2,
                                      ApplicantSoftSkills1,
                                      ApplicantSoftSkills2),
    % Weights for technical and soft skills
    WT = 0.6,
    WS = 0.4,
    % Evaluate total skills points
    SkillsPoints = WT * TechPoints + WS * SoftPoints,
    % Normalize the skills points
    NormalizedSkillsPoints = normalize_skills_points(SkillsPoints, WT, WS),
    NormalizedSkillsPointsFormatted =
        list_to_float(io_lib:format("~.2f", [NormalizedSkillsPoints])),
    {skills, SkillsPoints, NormalizedSkillsPointsFormatted}.

%% Helper function to calculate skill points for default mode
calculate_skill_points(JobSkill, JobSkil2, ApplicantSkill1, ApplicantSkill2) ->
    skill_match(JobSkill, ApplicantSkill1, ApplicantSkill2)
    + skill_match(JobSkil2, ApplicantSkill1, ApplicantSkill2).

%% Helper function to calculate skill points for strict mode
calculate_strict_skill_points(JobSkill, JobSkil2, ApplicantSkill1, ApplicantSkill2) ->
    strict_skill_match(JobSkill, ApplicantSkill1, ApplicantSkill2)
    * strict_skill_match(JobSkil2, ApplicantSkill1, ApplicantSkill2).

strict_skill_match(JobSkill, ApplicantSkill1, ApplicantSkill2) ->
    if JobSkill =:= ApplicantSkill1; JobSkill =:= ApplicantSkill2 ->
           1;
       true ->
           0
    end.

%% Helper function to normalize skills points
normalize_skills_points(SkillsPoints, WT, WS) ->
    SkillsPoints / (WT * 2 + WS * 2) * 100.

skill_match(JobSkill, ApplicantSkill1, ApplicantSkill2) ->
    case JobSkill of
        ApplicantSkill1 ->
            1;
        ApplicantSkill2 ->
            1;
        _ ->
            0
    end.

%% Total score evaluation function in default mode
score_eval(default, EduNormalized, ExpNormalized, SkillsNormalized) ->
    calculate_score(default, EduNormalized, ExpNormalized, SkillsNormalized);
%% Total score evaluation function in Strict mode
score_eval(strict, EduNormalized, ExpNormalized, SkillsNormalized) ->
    case has_invalid_values(EduNormalized, ExpNormalized, SkillsNormalized) of
        true ->
            {score, 0.0};
        false ->
            calculate_score(strict, EduNormalized, ExpNormalized, SkillsNormalized)
    end.

%% Helper function to check for zero or negative values
has_invalid_values(EduNormalized, ExpNormalized, SkillsNormalized) ->
    EduNormalized =< 0 orelse ExpNormalized =< 0 orelse SkillsNormalized =< 0.

%% Helper function to calculate the score
calculate_score(default, EduNormalized, ExpNormalized, SkillsNormalized) ->
    %% Arithmetic mean for default mode
    Score = (EduNormalized + ExpNormalized + SkillsNormalized) / 3,
    format_score(Score);
calculate_score(strict, EduNormalized, ExpNormalized, SkillsNormalized) ->
    %% Harmonic mean for strict mode
    Score = 3 / (1 / EduNormalized + 1 / ExpNormalized + 1 / SkillsNormalized),
    format_score(Score).

%% Helper function to format the score
format_score(Score) ->
    ScoreFormatted = list_to_float(io_lib:format("~.2f", [Score])),
    {score, ScoreFormatted}.

%% Star Rating evaluation function
star_eval(Score) ->
    %% Define the score ranges and corresponding star ratings
    StarRating =
        case true of
            _ when Score =< 0 ->
                0.0;
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
    %% @doc Creates Mnesia Table.
    %% @end
    case job_match_mnesia:start() of
        ok ->
            ok;
        _ ->
            io:format("Could not start Mnesia handler. Mnesia operations will not be performed~n")
    end,
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
            % The KB holds all facts in a list of tuples including the facts resulted from rules such as {mother, X, Y}
            % So The KB has the following pattern:
            % {job,JobID,
            %     {JName,
            %         {
            %             {education, JEduLevel, JEduField1, JEduField2},
            %             {experience, JExpYears, JExpField1, JExpField2},
            %             {techskills, JTechSkills1, JTechSkills2},
            %            {softskills, JSoftSkills1, JSoftSkills2}
            %         }
            %     }
            % }
            % {applicant,JobID,
            %     {ApplicantId,
            %         {ApplicantName,
            %             {education, ApplicantEduLevel, ApplicantEduField},
            %             {experience, ApplicantExpYears, ApplicantExpField1, ApplicantExpField2},
            %             {techskills, ApplicantTechSkills1, ApplicantTechSkills2},
            %             {softskills, ApplicantSoftSkills1, ApplicantSoftSkills2}
            %         }
            %     }
            % }
            % {strict,JobID,ApplicantId,
            %     {JName, ApplicantName,
            %         {score, ScoreFormatted},
            %         {stars, StarRating},
            %         {education, EducationPoints, NormalizedEduPointsFormatted},
            %         {experience, ExperiencePoints, NormalizedExpPointsFormatted},
            %         {skills, SkillsPoints, NormalizedSkillsPointsFormatted}
            %     }
            % }
            % {match,JobID,ApplicantId,
            %     {JName, ApplicantName,
            %         {score, ScoreFormatted},
            %         {stars, StarRating},
            %         {education, EducationPoints, NormalizedEduPointsFormatted},
            %         {experience, ExperiencePoints, NormalizedExpPointsFormatted},
            %         {skills, SkillsPoints, NormalizedSkillsPointsFormatted}
            %     }
            % }
            % Store KB in Mnesia table
            case job_match_mnesia:insert_kb(KB) of
                % KB stored in Mnesia table successfully.
                ok ->
                    ok;
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
