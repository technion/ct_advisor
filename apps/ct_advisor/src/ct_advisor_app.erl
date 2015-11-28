%%%-------------------------------------------------------------------
%% @doc ct_advisor public API
%% @end
%%%-------------------------------------------------------------------

-module('ct_advisor_app').

-behaviour(application).

%% Application callbacks
-export([start/2, 
        stop/1]).

%%====================================================================
%% API
%%====================================================================
setup_db() ->
    STH = ct_fetch:fetch_sth(), % TODO: Use previously saved disk value
    Latest = ct_fetch:parse_sth(STH),
    ets:insert(sth, {latest, Latest}),
    io:fwrite("First lookup: ~B~n", [Latest]).

start(_StartType, _StartArgs) ->
    setup_db(),
    db_connect:db_connect(),
    lager:set_loglevel(lager_console_backend, debug), % Debugging
    scheduler:start_link(),
    'ct_advisor_sup':start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    [{connector, C}] = ets:lookup(db, connector),
    lager:info("Connection to database shutdown: ~p", [C]),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
