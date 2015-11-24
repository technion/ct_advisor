%%%-------------------------------------------------------------------
%% @doc ct_advisor public API
%% @end
%%%-------------------------------------------------------------------

-module('ct_advisor_app').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================
setup() ->
    sth = ets:new(sth, [ named_table, public, {read_concurrency, true}]),
    users = ets:new(users, [ named_table, public, {read_concurrency, true}]),
    ets:insert(users, {"mail.xtraservice.de", "test@lolware.net"}),
    STH = ct_fetch:fetch_sth(), % TODO: Use previously saved disk value
    Latest = ct_fetch:parse_sth(STH),
    ets:insert(sth, {latest, Latest}),
    io:fwrite("First lookup: ~B~n", [Latest]).

start(_StartType, _StartArgs) ->
    setup(),
    scheduler:start_link(),
    'ct_advisor_sup':start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
