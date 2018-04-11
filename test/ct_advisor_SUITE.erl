-module(ct_advisor_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [no_update, update, domain_check].

querymock([]) ->
    {ok, "Columns", [{10502585}]};
querymock([A]) when is_integer(A) ->
    {ok, 1};
querymock([_A]) ->
    {ok, "Columns", [{<<"technion@lolware.net">>}]}.

init_per_suite(Config) ->
    application:start(ssl),
    application:start(ibrowse),
    Config.

no_update(_Config) ->
    K = "{\"tree_size\":10502585,\"timestamp\":1449817937400,\"sha256_root_hash\":\"qeBs0XUYqtWTMYTEbnIKQhQefv5eOCl+dZCFwPrpljk=\",\"tree_head_signature\":\"BAMARzBFAiEA5xzKR86R2jWkX67PBabhg1/v4GrfeeBEbK4bT4Npns0CIF9ew7he6hpMwbfsNDbZOnrzByo4EQcArov1jHQFBG0K\"}",
    meck:new(pgapp, [non_strict]),
    meck:expect(pgapp, equery, fun(_Query, Params) -> querymock(Params) end),
    meck:new(ct_fetch, [passthrough]),
    meck:expect(ct_fetch, fetch_sth, fun() -> K end),
    noupdate = leaf_iterate:scheduled_check(),
    meck:unload(pgapp),
    meck:unload(ct_fetch),
    ok.

update(_Config) ->
    K = "{\"tree_size\":10502586,\"timestamp\":1449817937400,\"sha256_root_hash\":\"qeBs0XUYqtWTMYTEbnIKQhQefv5eOCl+dZCFwPrpljk=\",\"tree_head_signature\":\"BAMARzBFAiEA5xzKR86R2jWkX67PBabhg1/v4GrfeeBEbK4bT4Npns0CIF9ew7he6hpMwbfsNDbZOnrzByo4EQcArov1jHQFBG0K\"}",
    meck:new(pgapp),
    meck:expect(pgapp, equery, fun(_Query, Params) -> querymock(Params) end),
    meck:new(ct_fetch, [passthrough]),
    meck:expect(ct_fetch, fetch_sth, fun() -> K end),
    [[{dNSName,"lolware.net"}, {dNSName,"www.lolware.net"},
        {serial,"19F169D2A081E71A79CE2219220D0B582D6"}]] =
        leaf_iterate:scheduled_check(),
    meck:unload(pgapp),
    meck:unload(ct_fetch),
    ok.

domain_check(_Config) ->
    meck:new(pgapp),
    meck:expect(pgapp, equery, fun(_Query, Params) -> querymock(Params) end),
    meck:new(ct_mail_alert),
    meck:expect(ct_mail_alert, send_alert, fun(Alerts, Domains, ID) ->
        [Alerts, Domains, ID] = [[{"lolware.net","technion@lolware.net"},
            {"www.lolware.net","technion@lolware.net"}],
            [{dNSName,"lolware.net"},{dNSName,"www.lolware.net"}],
            {serial,"19F169D2A081E71A79CE2219220D0B582D6"}] end),
    domain_parse:cert_domain_list([[{dNSName,"lolware.net"},
        {dNSName,"www.lolware.net"},
        {serial,"19F169D2A081E71A79CE2219220D0B582D6"}]]),
    true = meck:validate(ct_mail_alert),
    meck:unload(pgapp),
    meck:unload(ct_mail_alert),
    ok.

end_per_suite(_Config) ->
    ok.
