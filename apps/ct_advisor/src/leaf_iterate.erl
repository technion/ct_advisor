-module(leaf_iterate).
-compile([debug_info, export_all]).

scheduled_check() ->
    STH = ct_fetch:fetch_sth(),
    Latest = ct_fetch:parse_sth(STH),
    lookup_updates(Latest).

lookup_updates(Latest) ->
    case ets:lookup(sth, latest) of
    [{latest, LastLookup}] when Latest > LastLookup ->
        io:fwrite("Performing checks: ~B~n", [Latest]),
        Domains = run_checks(LastLookup, Latest),
        io:fwrite("Domains to verify: ~p~n", [Domains]);
    _ ->
        io:fwrite("No updates, latest still: ~B~n", [Latest])
    end.

run_checks(LOW, HIGH) ->
    {FROM, TO} = get_range(LOW, HIGH),
    io:fwrite("Running between: ~B and ~B~n", [FROM, TO]),
    Domains = enumerate_ids(FROM, TO),
    ets:insert(sth, {latest, TO+1}),
    Domains.

get_range(LOW, HIGH) when HIGH > LOW ->
    % Note the highest lookup should be STH -1
    % We also rate limit lookups per run
    case (HIGH - LOW) of
    Diff when Diff > 32 ->
        {LOW, LOW+32};
    _Diff ->
        {LOW, HIGH-1}
    end.
        

get_domain_from_id(ID) ->
    LeafEntry =  ct_fetch:fetch_entry(ID),
    MTL = leaf_parse:parse_leaf(LeafEntry),
    case leaf_parse:xparse(MTL) of
    none ->
        [];
    X509 ->
        leaf_parse:get_subjects(X509)
    end.

enumerate_ids(ID, ID) ->
    [get_domain_from_id(ID)];

enumerate_ids(FROM, TO) when FROM < TO ->
    [get_domain_from_id(FROM)| enumerate_ids(FROM+1, TO)].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("test_constants.hrl").

setup_table_test() ->
    sth = ets:new(sth, [ named_table, public, {read_concurrency, true}]),
    ets:insert(sth, {latest, 1024}). %Generally random starting example

range_test() ->
    ?assertEqual(get_range(7, 8), {7, 7}),
    ?assertEqual(get_range(7, 107), {7, 39}).

lookup_test() ->
    lookup_updates(1025),
    ?assertEqual(ets:lookup(sth, latest), [{latest,1025}]).

enumerate_test() ->
    ?assertEqual(enumerate_ids(9742371 , 9742372), ?TEST_ENUMERATED_DOMAINS).

-endif.
