-module(applicants_facts).

-export([extract_applicants_tuples/1]).

%% Purpose: Transforms a list of maps into a structured list of tuples accepted by SERESYE.
%% Argument: ListOfMaps - A list of maps obtained from processing BTM Jobs Ontology by Semantic Web ToolKit for Erlang applications.
%% Return: List of tuples representing applicants' facts.
extract_applicants_tuples(ListOfMaps) ->
    FilteredMaps = lists:filter(fun should_skip_map/1, ListOfMaps),
    Grouped =
        lists:foldl(fun(Map, Acc) -> group_by_job_seeker(Map, Acc) end, #{}, FilteredMaps),
    lists:map(fun({JobSeekerID, Data}) -> convert_to_tuple(JobSeekerID, Data) end,
              maps:to_list(Grouped)).

%% Purpose: Determines if a map should be included in the filtered list based on its 'p' key value.
%% Argument: Map - A map containing data of an applicant.
%% Return: Boolean indicating whether the map should be included (true) or skipped (false).
%% The return value (false or true) is used by the lists:filter function
%% to decide whether to exclude (false) or include (true) a map in the FilteredMaps.
should_skip_map(Map) ->
    case maps:get(p, Map, undefined) of
        {iri, <<"rdf">>, _} ->
            false;
        {iri, <<"rdfs">>, _} ->
            false;
        {iri, <<"owl">>, _} ->
            false;
        {iri, <<"http://ainf.aau.at/ontodebug#type">>} ->
            false;
        {iri, <<"http://ainf.aau.at/ontodebug#axiom">>} ->
            false;
        {iri, <<"http://ainf.aau.at/ontodebug#testCase">>} ->
            false;
        _ ->
            true
    end.

%% Purpose: Groups the maps by job seeker ID.
%% Argument: Map - A single map of applicant data, Acc - Accumulator for the grouped data.
%% Return: Updated accumulator containing the grouped data.
group_by_job_seeker(Map, Acc) ->
    #{s := {iri, IRI}} = Map,
    JobSeekerIDBinary = extract_job_seeker_id(IRI),
    maps:update_with(JobSeekerIDBinary, fun(OldData) -> [Map | OldData] end, [Map], Acc).

%% Purpose: Extracts the job seeker ID from an IRI.
%% Argument: IRI related to a job seeker.
%% Return: Extracted job seeker ID as a binary string.
extract_job_seeker_id(IRI) ->
    [_, JobSeekerIDBinary] = binary:split(IRI, <<"#">>, [global]),
    JobSeekerIDBinary.

%% Purpose: Converts grouped map data into a structured tuple.
%% Argument: JobSeekerID - The job seeker's ID, Maps - List of maps related to the job seeker.
%% Return: A tuple representing the job seeker's facts.
convert_to_tuple(JobSeekerID, Maps) ->
    Data = extract_job_seeker_data(Maps),
    JobPostingID = extract_job_posting_id(Maps),
    {applicant, JobPostingID, {JobSeekerID, Data}}.

%% Purpose: Extracts specific facts about the job seeker from the maps.
%% Argument: Maps - List of maps related to a job seeker.
%% Return: A tuple containing extracted facts.
extract_job_seeker_data(Maps) ->
    Name = extract_value(Maps, <<"hasNameDP">>),
    EduLevel = extract_value(Maps, <<"hasEduLevelDP">>),
    EduField = extract_value(Maps, <<"hasEduFieldDP">>),
    ExpYears = extract_value(Maps, <<"hasExpTimeDP">>),
    ExpField1 = extract_value(Maps, <<"hasExpFieldDP1">>),
    ExpField2 = extract_value(Maps, <<"hasExpFieldDP2">>),
    TechSkill1 = extract_value(Maps, <<"hasTechSkillsDP1">>),
    TechSkill2 = extract_value(Maps, <<"hasTechSkillsDP2">>),
    SoftSkill1 = extract_value(Maps, <<"hasSoftSkillsDP1">>),
    SoftSkill2 = extract_value(Maps, <<"hasSoftSkillsDP2">>),
    {Name,
     {education, EduLevel, EduField},
     {experience, ExpYears, ExpField1, ExpField2},
     {techskills, TechSkill1, TechSkill2},
     {softskills, SoftSkill1, SoftSkill2}}.

%% Purpose: Extracts the job posting ID that the job seeker applied to.
%% Argument: Maps - List of maps related to a job seeker.
%% Return: The job posting ID or a default value undefined if not found.
extract_job_posting_id(Maps) ->
    case lists:keyfind(<<"applyToJobPostingDP">>,
                       1,
                       lists:map(fun(Map) ->
                                    {proplabel(Map, <<"applyToJobPostingDP">>), propvalue(Map)}
                                 end,
                                 Maps))
    of
        {<<"applyToJobPostingDP">>, JobPostingID} ->
            JobPostingID;
        false ->
            undefined
    end.

%% Purpose: Extracts a specific value based on IRI suffix from a list of maps.
%% Argument: Maps - List of maps, IRI_suffix - Suffix of the IRI to match in the maps.
%% Return: Extracted value or undefined if not found.
extract_value(Maps, IRI_suffix) ->
    case lists:keyfind(IRI_suffix,
                       1,
                       lists:map(fun(Map) -> {proplabel(Map, IRI_suffix), propvalue(Map)} end,
                                 Maps))
    of
        {IRI_suffix, Value} ->
            Value;
        false ->
            undefined
    end.

%% Purpose: Extracts the property label from a map based on IRI suffix.
%% Argument: Map - A map containing data of an applicant, IRI_suffix - Suffix to match.
%% Return: Property label as a binary string.
proplabel(Map, IRI_suffix) ->
    #{p := {iri, IRI}} = Map,
    binary:part(IRI, {byte_size(IRI) - byte_size(IRI_suffix), byte_size(IRI_suffix)}).

%% Purpose: Extracts the value associated with the 'o' key in a map.
%% Argument: Map - A map containing data of an applicant.
%% Return: Value associated with the 'o' key.
propvalue(Map) ->
    #{o := Value} = Map,
    Value.
