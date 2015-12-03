-module(ct_mail_alert).
-compile([debug_info, export_all]).

-record(credentials, {hostname, username, password}).

%% Issues an email alert.

-spec send_alert([{string(), string()}], [tuple()]) -> ok.
send_alert([{Domain, User}], Certificate) ->
    lager:notice("We have an alert for ~p, ~p with cert ~p~n",
        [Domain, User, Certificate]),
    {ok, Config} = file:consult("priv/credentials.rr"),
    Creds = proplists:get_value(smtp, Config),
    {ok, Pid} = gen_smtp_client:send({"ctadvisor@lolware.net", [User],
        "Subject: SSL Has been issued for monitored domain\r\n"
        "From: ctadvisor@lolware.net\r\nTo: " ++ User ++ "\r\n\r\n"
        "ct_advisor has detected the issuance of an SSL certificate for domain "
        ++ Domain ++ " for which you are registered. If this was not you, you"
        " may wish to investigate."},
        [{relay, Creds#credentials.hostname},
        {username, Creds#credentials.username},
        {password, Creds#credentials.password}, {port, 587} ]),
    {ok, Pid}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

send_failmail_test() ->
    % In order to not flood mailboxes, this tests an attempt to send an email.
    % The test address is invalid - if we get the correct server rseponse
    % we assume we can succeed.
    {T, Pid} = send_alert([{"lolwaretest.net", "technion@invalidwillfail.com"}],
            [{dNSName, "www.lolwaretest.net"}, {dNSName, "lolwaretest.net"}]),
    ?assertEqual(ok, T),
    unlink(Pid),
    Monitor = erlang:monitor(process, Pid),
    Response = receive
    {'DOWN', Monitor, process, Pid, Error} ->
        Error
    after 5000 ->
        nomessage
    end,
    {error, no_more_hosts, {permanent_failure, _, SMTPError}} = Response,
    ?assertEqual(
        <<"554 Message rejected: Email address is not verified.\r\n">>,
        SMTPError). % TODO: After we actually verify with SES, this
                    % error will change.

send_mail_test() ->
    % In order to not flood mailboxes, this tests an attempt to send an email.
    % The test address is invalid - if we get the correct server rseponse
    % we assume we can succeed.
    {T, Pid} = send_alert([{"lolwaretest.net",
            "success@simulator.amazonses.com"}],
            [{dNSName, "www.lolwaretest.net"}, {dNSName, "lolwaretest.net"}]),
    ?assertEqual(ok, T),
    unlink(Pid),
    Monitor = erlang:monitor(process, Pid),
    Response = receive
    {'DOWN', Monitor, process, Pid, Error} ->
        Error
    after 5000 ->
        nomessage
    end,
    ?assertEqual(normal, Response).
-endif.
