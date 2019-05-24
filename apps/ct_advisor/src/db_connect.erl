-module(db_connect).
-export([db_connect/0]).

-record(credentials, {hostname, username, password}).

%% Establishes a connection to Postgresql.
db_connect() ->
    {ok, Config} = file:consult("priv/credentials.rr"),
    Creds = proplists:get_value(database, Config),
    {ok, C} = pgapp:connect([{size, 5}, 
            {host, Creds#credentials.hostname},
            {username, Creds#credentials.username},
            {password, Creds#credentials.password},
            {database, "ct_advisor_int_live"}]),
    lager:info("Connection to database started: ~p", [C]),
    ok.
    
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("test_constants.hrl"). 

db_connect_test() ->
    application:ensure_all_started(pgapp),
    db_connect(),
    application:stop(pgapp).

-endif.
 

