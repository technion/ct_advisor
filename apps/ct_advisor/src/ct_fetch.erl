-module(ct_fetch).
-compile([debug_info, export_all]).

-define(CT_LOG, "https://ct.googleapis.com/aviator").

%% Connect to the REST API and fetch the JSON entry for a particular node
-spec fetch_entry(pos_integer()) -> any().
fetch_entry(X) ->
    Xi = integer_to_list(X),
    URL = ?CT_LOG ++ "/ct/v1/get-entries?start=" ++ Xi ++ "&end=" ++ Xi,
    {ok, "200", _Headers, Content} =
        ibrowse:send_req(URL, [], get),
    Content.

%% Connect to the REST API and obtain the "sth", which is the total count
%% of certificates currently logged with the monitor.
-spec fetch_sth() -> any().
fetch_sth() ->
    {ok, "200", _Headers, Content} =
        ibrowse:send_req(?CT_LOG ++ "/ct/v1/get-sth", [], get),
    Content.

%% Extract the sth counter from the JSON response
-spec parse_sth(_) -> any().
parse_sth(JSON) ->
    {STH} = jiffy:decode(JSON),
    proplists:get_value(<<"tree_size">>, STH).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("test_constants.hrl").

%All tests based upon ID 9742371 - lolware.net
fetch_entry_test() ->
    ibrowse:start(),
    ?assertEqual(ct_fetch:fetch_entry(9742371), ?TEST_LEAF_ENTRY).

fetch_sth_test() ->
    ibrowse:start(),
    fetch_sth().

parse_sth() ->
    ?assertEqual(parse_sth(?TEST_STH), 9910235).
-endif.

