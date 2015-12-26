-module(domain_parse).
-export([cert_domain_list/1]).

% A list of certificates with a list of names
-spec cert_domain_list([any()]) -> 'ok'.
cert_domain_list(Domains) ->
    lager:info("Domain list for review: ~p" ,[Domains]),
    lists:foreach(fun(X) -> per_cert_domains(X) end, Domains).

% A list of domains for an individual certificate
-spec per_cert_domains([{'dNSName', _}]) -> 'ok'.
per_cert_domains(DomainsID) ->
    ID = proplists:lookup(serial, DomainsID),
    Domains = proplists:delete(serial, DomainsID),
    case lists:flatten(lookup_name_list(Domains)) of
    [] ->
        ok;
    Alerts ->
        ct_mail_alert:send_alert(Alerts, Domains, ID),
        ok
    end.

%% For a given domain name - check if it's registered in the datbaase
-spec lookup_name_list([{atom(), _}]) -> [[] | {_, _}].
lookup_name_list([]) ->
    [];

lookup_name_list([{dNSName, Name}|Tail]) ->
    {ok, _Columns, Rows} = pgapp:equery("SELECT email FROM registrations "
        "WHERE ($1 LIKE concat('%.',domain) or $1 = domain) AND active =1",
        [Name]),
    Match = case Rows of
    [{User}] ->
        {Name, binary_to_list(User)};
    _ ->
        []
    end,
    [Match|lookup_name_list(Tail)];

lookup_name_list([{_, _Name}|Tail]) ->
    %TIL: There are other types of subject names - see test suite
    [lookup_name_list(Tail)].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("test_constants.hrl").
lookup_fixture_test_() ->
    {setup, fun connect/0, fun teardown/1, fun lookup_name_listi/0}.

connect() ->
    application:ensure_all_started(pgapp),
    db_connect:db_connect(),
    ok.

teardown(_C) ->
    application:stop(pgapp).

lookup_name_listi() ->
    % using lists:flatten/1 because it is always called this way
    ?assertEqual(lists:flatten(lookup_name_list([])), []),
    ?assertEqual([], lists:flatten(lookup_name_list(?TEST_NONDNS_DOMAINS))),
    ?assertEqual([{"lolware.net","technion@lolware.net"},
            {"www.lolware.net","technion@lolware.net"}],
            lists:flatten(lookup_name_list(?TEST_LOOKUP_DOMAINS))).
-endif.
