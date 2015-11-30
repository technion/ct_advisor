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
start(_StartType, _StartArgs) ->
    db_connect:db_connect(),
    lager:set_loglevel(lager_console_backend, debug), % Debugging
    scheduler:start_link(),
    'ct_advisor_sup':start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    [{connector, C}] = ets:lookup(db, connector),
    ok = epgsql:close(C),
    ets:delete(db),
    lager:info("Connection to database shutdown: ~p", [C]),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
