-module(semantic_relatives_mnesia_handler).

-include_lib("stdlib/include/qlc.hrl").

-include("semantic_relatives_mnesia_kb.hrl").

-export([start/0, insert_kb/1, get_all_records/0, query_kb/2]).

-spec start() -> ok | error.
%% @doc Starts the Mnesia handler. It ensures that the tables are created and ready to use.
start() ->
    Node = node(),
    % Tries to create the Mnesia schema on the current node. If the schema already exists, it does nothing.
    case mnesia:create_schema([Node]) of
        ok ->
            ok;
        {error, {Node, {already_exists, Node}}} ->
            ok;
        Error ->
            Error
    end,
    % Starts Mnesia
    mnesia:start(),
    Tables = [semantic_relatives_kb],
    % Ensures the required tables are created.
    ensure_tables_created(Tables),
    % Waits for the tables to become available.
    case mnesia:wait_for_tables(Tables, 20000) of
        ok ->
            ok;
        {timeout, UnavailableTables} ->
            io:format("Timeout waiting for tables: ~p~n", [UnavailableTables]),
            error
    end.

% Ensures that the provided tables are created in Mnesia.
ensure_tables_created(Tables) ->
    [create_table_if_not_exists(Table) || Table <- Tables].

% Checks if a table exists in Mnesia. If it doesn't, it creates it.
create_table_if_not_exists(Table) ->
    case mnesia:table_info(Table, size) of
        {atomic, _Size} ->
            ok;
        {aborted, _Reason} ->
            create_table(Table);
        0 ->
            create_table(Table);
        undefined ->
            create_table(Table)
    end.

% Creates a table in Mnesia with the provided name.
create_table(Table) ->
    case mnesia:create_table(Table,
                             [{disc_copies, [node()]},
                              {attributes, record_info(fields, semantic_relatives_kb)},
                              {type, bag}])
    of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, Table}} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.

%% @doc Iterates over KB to inserts records into the semantic_relatives_kb table in Mnesia.
insert_kb(KB) ->
    lists:foreach(fun(Record) -> insert_record(Record) end, KB).

%% @doc Inserts a record into the semantic_relatives_kb table in Mnesia.
insert_record({Object, Subject}) ->
    Rec = #semantic_relatives_kb{object = Object, subject = Subject},
    Fun = fun() -> mnesia:write(Rec) end,
    {atomic, Val} = mnesia:transaction(Fun),
    case Val of
        ok ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end;
%% @doc Inserts a record into the semantic_relatives_kb table in Mnesia.
insert_record({Predicate, Object, Subject}) ->
    Rec = #semantic_relatives_kb{predicate = Predicate,
                                 object = Object,
                                 subject = Subject},
    Fun = fun() -> mnesia:write(Rec) end,
    {atomic, Val} = mnesia:transaction(Fun),
    case Val of
        ok ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.

-spec get_all_records() -> [#semantic_relatives_kb{}].
%% @doc Retrieves all records from the semantic_relatives_kb table in Mnesia.
get_all_records() ->
    Fun = fun() -> mnesia:match_object(#semantic_relatives_kb{_ = '_'}) end,
    {atomic, Records} = mnesia:transaction(Fun),
    case Records of
        [] ->
            {error, no_records};
        _ ->
            Records
    end.

-spec query_kb(atom(), {any(), any(), fun() | any()}) -> [any()].
%% @doc Performs a select operation on an Mnesia table using provided Predicate, Object, and Subject parameters.
%% When the Subject is a function, it filters the results using that function.
%% 'mnesia:select/2' is used with a match specification to select records from the table,
%% 'mnesia:select/2' takes as parameters the name of the table and a match specification, which is a list of match conditions,
%% Each match condition is a tuple with three elements:
%% a pattern that matches the records, a list of guard conditions (empty in this case []), and a list of result expressions,
%% In this case, the result expression ['$_'] means the whole record will be returned.
%% 'lists:filter/2' is used to filter records based on the provided function when Subject is a function,
%% 'lists:filter/2' takes a function and a list as parameters. It applies the function to every element in the list and
%% returns a new list that includes only the elements for which the function returns true.
%% 'lists:map/2' is used to map each record to a tuple of {Predicate, Object, Subject},
%% 'lists:map/2' applies a given function to each element of a list and returns a new list with the results.
%% In this case, it is used to map the {Table, Predicate, Object, Subject} records to {Predicate, Object, Subject} tuples.
%% We can call this function, for example, as follows:
%% semantic_relatives_mnesia_handler:query_kb(semantic_relatives_kb, {brother, '_', fun (X) -> (X == alice) or (X == anna) end}).
%% semantic_relatives_mnesia_handler:query_kb(semantic_relatives_kb, {grandfather, '_', fun (X) -> (X == caesar) or (X == anna) end}).
%% (Similar query_kb clauses that expect Predicate or Object as fun can be defined as in the query_kb below).
query_kb(Table, {Predicate, Object, SubjectFun}) when is_function(SubjectFun) ->
    MatchSpec = [{{Table, Predicate, Object, '_'}, [], ['$_']}],
    Fun = fun() -> mnesia:select(Table, MatchSpec) end,
    {atomic, Records} = mnesia:transaction(Fun),
    FilteredRecords =
        lists:filter(fun({_, _, _, Subject}) -> SubjectFun(Subject) end, Records),
    lists:map(fun({_Table, P, O, S}) -> {P, O, S} end, FilteredRecords);
%% We can call this function clause, for example, as follows:
%%  semantic_relatives_mnesia_handler:query_kb(semantic_relatives_kb, {brother, fun (X) -> (X == bob) or (X == mark) end, '_'}).
%% semantic_relatives_mnesia_handler:query_kb(semantic_relatives_kb, {parent, fun (X) -> (X == bob) or (X == mark) end, '_'}).
query_kb(Table, {Predicate, ObjectFun, Subject}) when is_function(ObjectFun) ->
    MatchSpec = [{{Table, Predicate, '_', Subject}, [], ['$_']}],
    Fun = fun() -> mnesia:select(Table, MatchSpec) end,
    {atomic, Records} = mnesia:transaction(Fun),
    FilteredRecords = lists:filter(fun({_, _, Object, _}) -> ObjectFun(Object) end, Records),
    lists:map(fun({_Table, P, O, S}) -> {P, O, S} end, FilteredRecords);
%% @doc This second clause of'query_kb' function performs a select operation on
%% an Mnesia table using provided Predicate, Object, and Subject parameters,
%% This clause does not expect a function in its arguments.
%% We can call this clause, for example, as follows:
%% semantic_relatives_mnesia_handler:query_kb(semantic_relatives_kb, {brother, '_', alice}).
%% semantic_relatives_mnesia_handler:query_kb(semantic_relatives_kb, {grandfather, '_', caesar}).
%% semantic_relatives_mnesia_handler:query_kb(semantic_relatives_kb, {mother, '_', alice}).
%% semantic_relatives_mnesia_handler:query_kb(semantic_relatives_kb, {father, '_', alice}).
query_kb(Table, {Predicate, Object, Subject}) ->
    MatchSpec = [{{Table, Predicate, Object, Subject}, [], ['$_']}],
    Fun = fun() -> mnesia:select(Table, MatchSpec) end,
    {atomic, Result} = mnesia:transaction(Fun),
    lists:map(fun({_Table, P, O, S}) -> {P, O, S} end, Result).
