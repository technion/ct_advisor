-module(leaf_iterate).
-compile([debug_info, export_all]).

-spec scheduled_check() -> 'ok'.
scheduled_check() ->
    STH = ct_fetch:fetch_sth(),
    Latest = ct_fetch:parse_sth(STH),
    lookup_updates(Latest).

-spec lookup_updates(_) -> 'ok'.
lookup_updates(Latest) ->
    case ets:lookup(sth, latest) of
    [{latest, LastLookup}] when Latest > LastLookup ->
        io:fwrite("Performing checks: ~B~n", [Latest]),
        Domains = run_checks(LastLookup, Latest),
        io:fwrite("Domains to verify: ~p~n", [Domains]);
    _ ->
        io:fwrite("No updates, latest still: ~B~n", [Latest])
    end.

-spec run_checks(number(),number()) -> [any(),...].
run_checks(LOW, HIGH) ->
    {FROM, TO} = get_range(LOW, HIGH),
    io:fwrite("Running between: ~B and ~B~n", [FROM, TO]),
    Domains = enumerate_ids(FROM, TO),
    ets:insert(sth, {latest, TO+1}),
    domain_parse:cert_domain_list(Domains),
    Domains.

-spec get_range(number(),number()) -> {number(),number()}.
get_range(LOW, HIGH) when HIGH > LOW ->
    % Note the highest lookup should be STH -1
    % We also rate limit lookups per run
    case (HIGH - LOW) of
    Diff when Diff > 32 ->
        {LOW, LOW+32};
    _Diff ->
        {LOW, HIGH-1}
    end.
        
-spec get_domain_from_id(_) -> any().
get_domain_from_id(ID) ->
    LeafEntry =  ct_fetch:fetch_entry(ID),
    MTL = leaf_parse:parse_leaf(LeafEntry),
    try leaf_parse:xparse(MTL) of
    X509 ->
        leaf_parse:get_subjects(X509)
    catch
    _:_ ->
        []
    end.

-spec enumerate_ids(_,_) -> [any(),...].
enumerate_ids(ID, ID) ->
    [get_domain_from_id(ID)];

enumerate_ids(FROM, TO) when FROM < TO ->
    [get_domain_from_id(FROM)| enumerate_ids(FROM+1, TO)].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("test_constants.hrl").

iterator_test() ->
    {setup, fun setup_table/0, fun teardown/1, [fun ranges/0,
        fun lookup/0, fun enumerate/0]}.

setup_table() ->
    sth = ets:new(sth, [ named_table, public, {read_concurrency, true}]),
    users = ets:new(users, [ named_table, public, {read_concurrency, true}]),
    ets:insert(sth, {latest, 1024}). %Generally random starting example

teardown(_) ->
    ets:delete(sth),
    ets:delete(users).

ranges() ->
    ?assertEqual({7, 7}, get_range(7, 8)),
    ?assertEqual({7, 39}, get_range(7, 107)).

lookup() ->
    lookup_updates(1025),
    ?assertEqual([{latest,1025}], ets:lookup(sth, latest)).

enumerate() ->
    ?assertEqual(?TEST_ENUMERATED_DOMAINS, enumerate_ids(9742371 , 9742372)).

-endif.
