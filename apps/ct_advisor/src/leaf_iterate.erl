-module(leaf_iterate).
-export([scheduled_check/0]).
%enumberate_ids/2 is not a programatically required export.
% It is often required for debugging however.
%-export([enumerate_ids/2]).

%% The entry function. This checks the latest recorded certificate and fires
%% processing based on that.
-spec scheduled_check() -> 'noupdate' | 'updated'.
scheduled_check() ->
    STH = ct_fetch:fetch_sth(),
    Latest = ct_fetch:parse_sth(STH),
    lookup_updates(Latest).

% Compares the input STH with the last checked value based on database lookup.
% Calls new checks as appropriate.
-spec lookup_updates(_) -> 'noupdate' | 'updated'.
lookup_updates(Latest) ->
    [{connector, C}] = ets:lookup(db, connector),
    {ok, _Columns, Rows} = epgsql:equery(C, "SELECT latest FROM STH"),
    case Rows of
    [{LastLookup}] when Latest > LastLookup ->
        lager:info("Performing checks: ~B~n", [Latest]),
        Domains = run_checks(LastLookup, Latest),
        lager:info("Domains to verify: ~p~n", [Domains]),
        updated;
    _ ->
        lager:debug("No updates, latest still: ~B~n", [Latest]),
        noupdate
    end.

%% Downloads and parses a a range of certificate id's, updates
%% last checked value in database
-spec run_checks(number(), number()) -> [any(), ...].
run_checks(LOW, HIGH) ->
    {FROM, TO} = get_range(LOW, HIGH),
    lager:info("Running between: ~B and ~B~n", [FROM, TO]),
    Domains = enumerate_ids(FROM, TO),
    [{connector, C}] = ets:lookup(db, connector),
    {ok, 1} = epgsql:equery(C, "UPDATE sth SET latest = $1", [TO+1]),
    domain_parse:cert_domain_list(Domains),
    Domains.

%% Rate limiting function - if a range is higher than a configured
%% value - reduce the range.
-spec get_range(number(), number()) -> {number(), number()}.
get_range(LOW, HIGH) when HIGH > LOW ->
    % Note the highest lookup should be STH -1
    % We also rate limit lookups per run
    case (HIGH - LOW) of
    Diff when Diff > 32 ->
        {LOW, LOW+32};
    _Diff ->
        {LOW, HIGH-1}
    end.

%% Will fetch a certifcicate and use the various parsing functions to
%% extract a list of domains on that certificate.
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

-spec enumerate_ids(_, _) -> [any(), ...].
enumerate_ids(ID, ID) ->
    [get_domain_from_id(ID)];

enumerate_ids(FROM, TO) when FROM < TO ->
    [get_domain_from_id(FROM)| enumerate_ids(FROM+1, TO)].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("test_constants.hrl").

lookup_fixture_test_() ->
    {setup, fun connect/0, fun teardown/1, fun lookup/0}.

connect() ->
    db_connect:db_connect(),
    [{connector, C}] = ets:lookup(db, connector),
    C.

teardown(C) ->
    epgsql:close(C),
    ets:delete(db).

ranges_test() ->
    ?assertEqual({7, 7}, get_range(7, 8)),
    ?assertEqual({7, 39}, get_range(7, 107)).

lookup() ->
    ?assertEqual(noupdate, lookup_updates(1025)).

enumerate_test() ->
    ?assertEqual(?TEST_ENUMERATED_DOMAINS, enumerate_ids(9742371 , 9742372)).

-endif.
