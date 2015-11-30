-module(db_connect).
-export([db_connect/0]).

-record(credentials, {hostname, username, password}).

%% Establishes a connection to Postgresql.
%% Creates an ETS table to record the connection PID
db_connect() ->
    {ok, [Creds|_Empty]} = file:consult("priv/credentials.rr"),
    db = ets:new(db, [ named_table, public, {read_concurrency, true}]),
    {ok, C} = epgsql:connect(Creds#credentials.hostname,
            Creds#credentials.username, Creds#credentials.password,
            [{database, "ct_advisor"}, {timeout, 4000}]),
    ets:insert(db, {connector, C}),
    lager:info("Connection to database started: ~p", [C]),
    ok.

