-module(sendstats).
-export([gap/2]).

-ifdef(TEST).
-define(LATESTMETRIC, <<"testapp.testvalue">>).
-define(CURRENTMETRIC, <<"testapp.testvalue2">>).
-else.
-define(LATESTMETRIC, <<"ctadvisor.latest">>).
-define(CURRENTMETRIC, <<"ctadvisor.current">>).
-endif.

-spec gap(pos_integer(), pos_integer()) -> ok.
gap(Latest, Current) ->
    F = fun() -> gap_process(Latest, Current) end,
    spawn(F),
    ok.

-spec gap_process(pos_integer(), pos_integer()) -> any().
gap_process(Latest, Current) ->
    statsderl:gauge(?LATESTMETRIC, Latest, 0.02),
    statsderl:gauge(?CURRENTMETRIC, Current, 0.02).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

gap_test() ->
    statsderl_app:start(),
    gap(30, 40).
-endif.

