-module(jobs_facts).

-export([extract_jobs_tuples/1]).

%% Purpose: Extracts jobs facts from a list of maps and converts it into a structured list of tuples accepted by SERESYE.
%% Argument: ListOfMaps - A list of maps obtained from processing BTM Jobs Ontology by Semantic Web ToolKit for Erlang applications.
%% Return: List of tuples representing jobs' facts.
extract_jobs_tuples(ListOfMaps) ->
    FilteredMaps = filter_unwanted_maps(ListOfMaps),
    {JobNameMap, OtherMaps} =
        lists:partition(fun(Map) -> is_job_name_map(Map) end, FilteredMaps),
    BaseURL = extract_base_url(JobNameMap),
    JobNames = lists:map(fun(Map) -> extract_job_name_and_id(Map) end, JobNameMap),
    Grouped = group_by_job_name(OtherMaps, JobNames, BaseURL),
    lists:map(fun({JobPostingID, JobName, Data}) ->
                 convert_to_job_tuple(JobPostingID, JobName, Data)
              end,
              Grouped).

%% Purpose: Filters out maps that are not relevant to job data.
%% Argument: Maps - List of maps to be filtered.
%% Return: Filtered list of maps.
filter_unwanted_maps(Maps) ->
    lists:filter(fun(Map) -> not is_unwanted_map(Map) end, Maps).

%% Purpose: Determines if a map is unwanted based on its properties.
%% Argument: Map - Map to be evaluated.
%% Return: Boolean indicating if the map is unwanted (true).
%% The return value (true or false) is used by the lists:filter function
%% to decide whether to exclude (true) or include (false).
is_unwanted_map(Map) ->
    case maps:get(p, Map, undefined) of
        {iri, <<"rdf">>, _} ->
            true;
        {iri, <<"rdfs">>, _} ->
            true;
        {iri, <<"owl">>, _} ->
            true;
        {iri, <<"http://ainf.aau.at/ontodebug#type">>} ->
            true;
        {iri, <<"http://ainf.aau.at/ontodebug#axiom">>} ->
            true;
        {iri, <<"http://ainf.aau.at/ontodebug#testCase">>} ->
            true;
        _ ->
            false
    end.

%% Purpose: Checks if a map contains a job name based on 'p' key suffix: 'requireBTMJobsTitleDP'.
%% Argument: Map - Map to be evaluated.
%% Return: Boolean indicating if the map contains a job name.
is_job_name_map(Map) ->
    case Map of
        #{p := {iri, IRI}} ->
            binary:part(IRI,
                        {byte_size(IRI) - byte_size(<<"requireBTMJobsTitleDP">>),
                         byte_size(<<"requireBTMJobsTitleDP">>)})
            == <<"requireBTMJobsTitleDP">>;
        _ ->
            false
    end.

%% Purpose: Extracts the job name and job posting ID from a map.
%% Argument: Map - Map containing job name and job posting ID.
%% Return: Tuple with job name and job posting ID.
extract_job_name_and_id(Map) ->
    #{s := {iri, JobPostingIRI}, o := JobName} = Map,
    [_, JobPostingID] = binary:split(JobPostingIRI, <<"#">>, [global]),
    {JobName, JobPostingID}.

%% Purpose: Groups job data by job name.
%% Argument: Maps - List of maps to be grouped.
%% Argument: JobNames - List of job names and job posting IDs.
%% Argument: BaseURL: Base URL for constructing IRI.
%% Return: Grouped job data.
group_by_job_name(Maps, JobNames, BaseURL) ->
    lists:foldl(fun(JobName, Acc) -> group_job_data(JobName, Maps, Acc, BaseURL) end,
                [],
                JobNames).

%% Purpose: Finds the base URL from a list of maps.
%% Argument: JobNameMap: List of maps containing job names.
%% Return: Base URL binary string.
extract_base_url(JobNameMap) ->
    find_base_url(JobNameMap).

find_base_url([]) ->
    undefined;
find_base_url([#{s := {iri, IRI}} | _]) ->
    [BaseURL, _] = binary:split(IRI, <<"#">>, [global]),
    BaseURL;
find_base_url([_ | T]) ->
    find_base_url(T).

%% Purpose: Groups job-specific facts.
%% Argument:JobName - Job name to filter data by.
%% Argument: Maps - List of maps with job data.
%% Argument: Acc - Accumulator for grouped data.
%% Argument: BaseURL: Base URL for constructing IRI.
%% Return: Grouped job data for a specific job name.
group_job_data({JobName, JobPostingID}, Maps, Acc, BaseURL) ->
    JobData =
        lists:filter(fun(Map) ->
                        maps:get(s, Map, undefined)
                        == {iri, <<BaseURL/binary, "#", JobName/binary>>}
                     end,
                     Maps),
    [{JobPostingID, JobName, extract_job_posting_data(JobData)} | Acc].

%% Purpose: Extracts detailed job data from a list of maps.
%% Argument: Maps - List of maps with job data.
%% Return: Structured tuple of job data.
extract_job_posting_data(Maps) ->
    EduLevel = extract_value(Maps, <<"requireEduLevelDP">>),
    EduField1 = extract_value(Maps, <<"requireEduFieldDP1">>),
    EduField2 = extract_value(Maps, <<"requireEduFieldDP2">>),
    ExpYears = extract_value(Maps, <<"requireExpTimeDP">>),
    ExpField1 = extract_value(Maps, <<"requireExpFieldDP1">>),
    ExpField2 = extract_value(Maps, <<"requireExpFieldDP2">>),
    TechSkill1 = extract_value(Maps, <<"requireTechSkillsDP1">>),
    TechSkill2 = extract_value(Maps, <<"requireTechSkillsDP2">>),
    SoftSkill1 = extract_value(Maps, <<"requireSoftSkillsDP1">>),
    SoftSkill2 = extract_value(Maps, <<"requireSoftSkillsDP2">>),
    {{education, EduLevel, EduField1, EduField2},
     {experience, ExpYears, ExpField1, ExpField2},
     {techskills, TechSkill1, TechSkill2},
     {softskills, SoftSkill1, SoftSkill2}}.

%% Purpose: Converts job data into a tuple format.
%% Argument: JobPostingID - Job posting ID.
%% Argument: JobName - Name of the job.
%% Argument: Data - Job-specific data.
%% Return: Tuple representing a job facts.
convert_to_job_tuple(JobPostingID, JobName, Data) ->
    {job, JobPostingID, {JobName, Data}}.

%% Purpose: Extracts a value associated with a specific IRI suffix from a list of maps.
%% Argument: Maps - List of maps to search.
%% Argument: IRI_suffix - IRI suffix to match.
%% Return: Value associated with the IRI suffix.
extract_value(Maps, IRI_suffix) ->
    case lists:keyfind(IRI_suffix,
                       1,
                       lists:map(fun(Map) -> {proplabel(Map, IRI_suffix), propvalue(Map)} end,
                                 Maps))
    of
        {IRI_suffix, Value} ->
            Value;
        false ->
            undefined % Default value in case of no match
    end.

%% Purpose: Extracts the property 'p' label from a map based on 'p' IRI suffix.
%% Argument: Map - Map containing the property 'p'.
%% Argument: IRI_suffix: 'p' IRI suffix to match.
%% Return: Property 'p' label.
proplabel(Map, IRI_suffix) ->
    #{p := {iri, IRI}} = Map,
    binary:part(IRI, {byte_size(IRI) - byte_size(IRI_suffix), byte_size(IRI_suffix)}).

%% Purpose: Extracts the property 'o' value from a map.
%% Argument: Map - Map containing the 'o' value.
%% Return: Property 'o' value.
propvalue(Map) ->
    #{o := Value} = Map,
    Value.
