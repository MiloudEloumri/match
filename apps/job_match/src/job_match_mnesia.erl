%% @file job_match_mnesia.erl
%% @brief Mnesia database operations for job matching application.
%% @description This module defines the functions for interacting with the Mnesia database,
%% including starting the database, ensuring tables are created, inserting knowledge base (KB)
%% records, and retrieving various types of records such as jobs, applicants, and match results.
-module(job_match_mnesia).

-include_lib("stdlib/include/qlc.hrl").

-include("job_match_mnesia_kb.hrl").

%% Exported functions
-export([start/0, insert_kb/1, get_all_records/0, get_job/1, get_applicant/1,
         get_applicant/2, get_match/1, get_match/2, get_strict/1, get_strict/2,
         get_all_results_by_job_and_applicant/2, get_all_results_by_applicant/1,
         get_all_results_by_job/1]).

%% @doc Starts Mnesia and ensures the required tables are created and ready for use.
-spec start() -> ok | error.
start() ->
    Node = node(),
    % Create Mnesia schema if it doesn't exist
    case mnesia:create_schema([Node]) of
        ok ->
            ok;
        {error, {Node, {already_exists, Node}}} ->
            ok;
        Error ->
            Error
    end,
    % Start Mnesia
    mnesia:start(),
    Tables = [job, applicant, match, strict],
    % Ensure all required tables are created
    ensure_tables_created(Tables),
    % Wait for tables to become available
    case mnesia:wait_for_tables(Tables, 20000) of
        ok ->
            ok;
        {timeout, UnavailableTables} ->
            io:format("Timeout waiting for tables: ~p~n", [UnavailableTables]),
            error
    end.

%% @doc Ensures that all specified tables are created in Mnesia.
ensure_tables_created(Tables) ->
    [create_table(Table) || Table <- Tables].

%% @doc Creates a specific table in Mnesia with the necessary attributes.
create_table(Table) ->
    Attributes = get_table_attributes(Table),
    case mnesia:create_table(Table,
                             [{disc_copies, [node()]}, {attributes, Attributes}, {type, bag}])
    of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, Table}} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.

%% @doc Retrieves the attributes for each table based on the record definition.
get_table_attributes(job) ->
    record_info(fields, job);
get_table_attributes(applicant) ->
    record_info(fields, applicant);
get_table_attributes(match) ->
    record_info(fields, match);
get_table_attributes(strict) ->
    record_info(fields, strict).

%% @doc Inserts a list of KB records into Mnesia.
-spec insert_kb(list()) -> ok | {error, any()}.
insert_kb(KB) ->
    lists:foreach(fun(Record) -> insert_record(Record) end, KB).

%% @doc Inserts a single record into the appropriate Mnesia table based on its type.
-spec insert_record(tuple()) -> ok | {error, any()}.
insert_record({job,
               JobID,
               {JobName, {EducationTuple, ExperienceTuple, TechSkillsTuple, SoftSkillsTuple}}}) ->
    % Extract fields from the tuples
    {education, JEduLevel, JEduField1, JEduField2} = EducationTuple,
    {experience, JExpYears, JExpField1, JExpField2} = ExperienceTuple,
    {techskills, JTechSkills1, JTechSkills2} = TechSkillsTuple,
    {softskills, JSoftSkills1, JSoftSkills2} = SoftSkillsTuple,
    % Create the record
    Rec = #job{job_id = JobID,
               job_name = JobName,
               education =
                   #education{level = JEduLevel,
                              field1 = JEduField1,
                              field2 = JEduField2},
               experience =
                   #experience{years = JExpYears,
                               field1 = JExpField1,
                               field2 = JExpField2},
               techskills = #techskills{skill1 = JTechSkills1, skill2 = JTechSkills2},
               softskills = #softskills{skill1 = JSoftSkills1, skill2 = JSoftSkills2}},
    maybe_insert_record(job, Rec);
insert_record({applicant,
               JobID,
               {ApplicantID,
                {ApplicantName,
                 {education, ApplicantEduLevel, ApplicantEduField},
                 {experience, ApplicantExpYears, ApplicantExpField1, ApplicantExpField2},
                 {techskills, ApplicantTechSkills1, ApplicantTechSkills2},
                 {softskills, ApplicantSoftSkills1, ApplicantSoftSkills2}}}}) ->
    % Create the applicant record
    Rec = #applicant{job_id = JobID,
                     applicant_id = ApplicantID,
                     applicant_name = ApplicantName,
                     education = #education{level = ApplicantEduLevel, field1 = ApplicantEduField},
                     experience =
                         #experience{years = ApplicantExpYears,
                                     field1 = ApplicantExpField1,
                                     field2 = ApplicantExpField2},
                     techskills =
                         #techskills{skill1 = ApplicantTechSkills1, skill2 = ApplicantTechSkills2},
                     softskills =
                         #softskills{skill1 = ApplicantSoftSkills1, skill2 = ApplicantSoftSkills2}},
    maybe_insert_record(applicant, Rec);
insert_record({match,
               JobID,
               ApplicantID,
               {JobName,
                ApplicantName,
                {score, ScoreFormatted},
                {stars, StarRating},
                {education, EducationPoints, NormalizedEduPointsFormatted},
                {experience, ExperiencePoints, NormalizedExpPointsFormatted},
                {skills, SkillsPoints, NormalizedSkillsPointsFormatted}}}) ->
    % Create the match record
    Rec = #match{job_id = JobID,
                 applicant_id = ApplicantID,
                 job_name = JobName,
                 applicant_name = ApplicantName,
                 score = #score{value = ScoreFormatted},
                 stars = #stars{value = StarRating},
                 education_eval =
                     #education_eval{points = EducationPoints,
                                     normalized_points = NormalizedEduPointsFormatted},
                 experience_eval =
                     #experience_eval{points = ExperiencePoints,
                                      normalized_points = NormalizedExpPointsFormatted},
                 skills_eval =
                     #skills_eval{points = SkillsPoints,
                                  normalized_points = NormalizedSkillsPointsFormatted}},
    maybe_insert_record(match, Rec);
insert_record({strict,
               JobID,
               ApplicantID,
               {JobName,
                ApplicantName,
                {score, ScoreFormatted},
                {stars, StarRating},
                {education, EducationPoints, NormalizedEduPointsFormatted},
                {experience, ExperiencePoints, NormalizedExpPointsFormatted},
                {skills, SkillsPoints, NormalizedSkillsPointsFormatted}}}) ->
    % Create the strict record
    Rec = #strict{job_id = JobID,
                  applicant_id = ApplicantID,
                  job_name = JobName,
                  applicant_name = ApplicantName,
                  score = #score{value = ScoreFormatted},
                  stars = #stars{value = StarRating},
                  education_eval =
                      #education_eval{points = EducationPoints,
                                      normalized_points = NormalizedEduPointsFormatted},
                  experience_eval =
                      #experience_eval{points = ExperiencePoints,
                                       normalized_points = NormalizedExpPointsFormatted},
                  skills_eval =
                      #skills_eval{points = SkillsPoints,
                                   normalized_points = NormalizedSkillsPointsFormatted}},
    maybe_insert_record(strict, Rec).

%% Helper function to insert record if it doesn't already exist
-spec maybe_insert_record(atom(), tuple()) -> ok | {error, any()}.
maybe_insert_record(Table, Rec) ->
    Key = case Table of
              job ->
                  {Rec#job.job_id};
              applicant ->
                  {Rec#applicant.job_id, Rec#applicant.applicant_id};
              match ->
                  {Rec#match.job_id, Rec#match.applicant_id};
              strict ->
                  {Rec#strict.job_id, Rec#strict.applicant_id}
          end,
    % Check if record already exists
    {atomic, Exists} = mnesia:transaction(fun() -> mnesia:read({Table, Key}) end),
    case Exists of
        [] -> % If not exists, insert it
            write_to_mnesia(Table, Rec);
        _ -> % If exists, skip insertion
            ok
    end.

%% @doc Generic function to write a record to a Mnesia table.
write_to_mnesia(Table, Rec) ->
    Fun = fun() -> mnesia:write(Table, Rec, write) end,
    case mnesia:transaction(Fun) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.

%% @doc Retrieves all records from all Mnesia tables.
-spec get_all_records() -> list() | {error, no_records}.
get_all_records() ->
    Fun = fun() ->
             [mnesia:match_object(#job{_ = '_'}),
              mnesia:match_object(#applicant{_ = '_'}),
              mnesia:match_object(#match{_ = '_'}),
              mnesia:match_object(#strict{_ = '_'})]
          end,
    {atomic, Records} = mnesia:transaction(Fun),
    case Records of
        [] ->
            {error, no_records};
        _ ->
            lists:flatten(Records)
    end.

%% @doc Retrieves a job record by its JobID.
get_job(JobID) ->
    {atomic, Result} =
        mnesia:transaction(fun() -> mnesia:match_object(#job{job_id = JobID, _ = '_'}) end),
    Result.

%% @doc Retrieves an applicant record by their ApplicantID.
get_applicant(ApplicantID) ->
    {atomic, Result} =
        mnesia:transaction(fun() ->
                              mnesia:match_object(#applicant{job_id = '_',
                                                             applicant_id = ApplicantID,
                                                             _ = '_'})
                           end),
    Result.

%% @doc Retrieves an applicant record by JobID and ApplicantID.
get_applicant(JobID, ApplicantID) ->
    {atomic, Result} =
        mnesia:transaction(fun() ->
                              mnesia:match_object(#applicant{job_id = JobID,
                                                             applicant_id = ApplicantID,
                                                             _ = '_'})
                           end),
    Result.

%% @doc Retrieves all match records for a specific JobID.
get_match(JobID) ->
    {atomic, Result} =
        mnesia:transaction(fun() ->
                              mnesia:match_object(#match{job_id = JobID,
                                                         applicant_id = '_',
                                                         _ = '_'})
                           end),
    Result.

%% @doc Retrieves a specific match record by JobID and ApplicantID.
get_match(JobID, ApplicantID) ->
    {atomic, Result} =
        mnesia:transaction(fun() ->
                              mnesia:match_object(#match{job_id = JobID,
                                                         applicant_id = ApplicantID,
                                                         _ = '_'})
                           end),
    Result.

%% @doc Retrieves all strict match records for a specific JobID.
get_strict(JobID) ->
    {atomic, Result} =
        mnesia:transaction(fun() ->
                              mnesia:match_object(#strict{job_id = JobID,
                                                          applicant_id = '_',
                                                          _ = '_'})
                           end),
    Result.

%% @doc Retrieves a specific strict match record by JobID and ApplicantID.
get_strict(JobID, ApplicantID) ->
    {atomic, Result} =
        mnesia:transaction(fun() ->
                              mnesia:match_object(#strict{job_id = JobID,
                                                          applicant_id = ApplicantID,
                                                          _ = '_'})
                           end),
    Result.

%% @doc Retrieves both match and strict match results for a specific JobID.
-spec get_all_results_by_job(binary()) ->
                                {match_results, list()} | {strict_results, list()}.
get_all_results_by_job(JobID) ->
    MatchResults = get_match(JobID),
    StrictResults = get_strict(JobID),
    {MatchResults, StrictResults}.

%% @doc Retrieves both match and strict match results for a specific ApplicantID.
-spec get_all_results_by_applicant(binary()) ->
                                      {match_results, list()} | {strict_results, list()}.
get_all_results_by_applicant(ApplicantID) ->
    {atomic, MatchResults} =
        mnesia:transaction(fun() ->
                              mnesia:match_object(#match{job_id = '_',
                                                         applicant_id = ApplicantID,
                                                         _ = '_'})
                           end),
    {atomic, StrictResults} =
        mnesia:transaction(fun() ->
                              mnesia:match_object(#strict{job_id = '_',
                                                          applicant_id = ApplicantID,
                                                          _ = '_'})
                           end),
    {MatchResults, StrictResults}.

%% @doc Retrieves both match and strict match results for a specific JobID and ApplicantID.
-spec get_all_results_by_job_and_applicant(binary(), binary()) ->
                                              {match_result, list()} | {strict_result, list()}.
get_all_results_by_job_and_applicant(JobID, ApplicantID) ->
    MatchResult = get_match(JobID, ApplicantID),
    StrictResult = get_strict(JobID, ApplicantID),
    {MatchResult, StrictResult}.%% @doc Example Queries:
                                %% Start the application and test various queries.
                                %% @example rebar3 shell
                                %% @example job_match:start().
                                %% @example job_match_mnesia:get_job(<<"JPosting11">>).
                                %% @example job_match_mnesia:get_applicant(<<"JSeeker63">>).
                                %% @example job_match_mnesia:get_applicant(<<"JPosting11">>, <<"JSeeker63">>).
                                %% @example job_match_mnesia:get_match(<<"JPosting11">>).
                                %% @example job_match_mnesia:get_match(<<"JPosting11">>, <<"JSeeker63">>).
                                %% @example job_match_mnesia:get_strict(<<"JPosting11">>).
                                %% @example job_match_mnesia:get_strict(<<"JPosting11">>, <<"JSeeker63">>).
                                %% @example job_match_mnesia:get_all_results_by_job(<<"JPosting11">>).
                                %% @example job_match_mnesia:get_all_results_by_applicant(<<"JSeeker63">>).
                                %% @example job_match_mnesia:get_all_results_by_job_and_applicant(<<"JPosting11">>, <<"JSeeker63">>).
                                %% @example job_match_mnesia:get_all_records().
