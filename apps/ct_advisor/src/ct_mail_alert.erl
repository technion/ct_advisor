-module(ct_mail_alert).
-export([send_alert/3]).

-record(credentials, {hostname, username, password}).

%% Issues an email alert.

-spec send_alert([{string(), string()}], [tuple()], {serial, string()}) ->
    {'ok', pid()}.
send_alert([{Domain, User}|Tail], Certificate, {serial, Serial}) ->
    lager:notice("We have an alert for ~p, ~p with cert ~p~n",
        [Domain, User, Certificate]),
    {ok, Config} = file:consult("priv/credentials.rr"),
    Creds = proplists:get_value(smtp, Config),
    {ok, Pid} = gen_smtp_client:send({"ctadvisor@lolware.net", [User],
        "Subject: SSL Has been issued for monitored domain\r\n"
        "From: ctadvisor@lolware.net\r\nTo: " ++ User ++ "\r\n\r\n"
        "ct_advisor has detected the issuance of an SSL certificate for domain "
        ++ Domain ++ " for which you are registered. If this was not you, you"
        " may wish to investigate. You can obtain further information "
        "by reviewing the issued certificate here: https://crt.sh/?serial="
        ++ Serial},
        [{relay, Creds#credentials.hostname},
        {username, Creds#credentials.username},
        {password, Creds#credentials.password}, {port, 587} ]),
    % While it feels more idiomatic, defining send_alert([]..) had numerous
    % complications.
    case Tail =/= [] of
    true ->
        send_alert(Tail, Certificate, {serial, Serial});
    _ ->
        ok
    end,
    {ok, Pid}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

send_bouncemail_test() ->
    % AWS SES will still accept a bounce - but should generate an alert to SNS
    {T, Pid} = send_alert([{"lolwaretest.net",
            "bounce@simulator.amazonses.com"}],
            [{dNSName, "www.lolwaretest.net"}, {dNSName, "lolwaretest.net"}],
            {serial, "19F169D2A081E71A79CE2219220D0B582D6"}),
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

send_mail_test() ->
    {T, Pid} = send_alert([{"lolware.net",
            "success@simulator.amazonses.com"}, 
            {"www.lolware.net", "success@simulator.amazonses.com"}],
            [{dNSName, "www.lolwaretest.net"}, {dNSName, "lolwaretest.net"}],
            {serial, "19F169D2A081E71A79CE2219220D0B582D6"}),
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
