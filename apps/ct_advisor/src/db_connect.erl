-module(db_connect).
-export([db_connect/0]).

db_connect() ->
    db = ets:new(db, [ named_table, public, {read_concurrency, true}]),
    {ok, C} = epgsql:connect("localhost", "ct_advisor", "erwgreg87uyt$",
            [{database, "ct_advisor"}, {timeout, 4000}]), % Password will be changed by the time you see this in github. Don't waste your time..
    ets:insert(db, {connector, C}),
    lager:info("Connection to database started: ~p", [C]).

