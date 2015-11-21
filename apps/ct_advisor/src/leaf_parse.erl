-module(leaf_parse).
-compile([debug_info, export_all]).

-include_lib("public_key/include/public_key.hrl"). 

-spec parse_leaf(_) -> any().
parse_leaf(RAW) ->
    {JSON} = jiffy:decode(RAW),
    [{Entries}] = proplists:get_value(<<"entries">>, JSON),
    proplists:get_value(<<"leaf_input">>, Entries).

-spec xparse(binary()) -> any().
xparse(MerkleLeafB64) ->
    MerkleLeafBin = base64:decode(MerkleLeafB64),
    <<_Version:8, _LeafType:8, _Timestamp:64, _LogType:16, 
        _ASNLen:24, Cert/binary>> = MerkleLeafBin,

    public_key:pkix_decode_cert(Cert, otp).

-spec get_subjects(#'OTPCertificate'{tbsCertificate::#'OTPTBSCertificate'{extensions::[any()]}}) -> any().
get_subjects(Cert) ->
    Names = [Ext || Ext <- 
        Cert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.extensions,
        Ext#'Extension'.extnID == {2,5,29,17} ],
    S = lists:last(Names), % Should only be one element
    S#'Extension'.extnValue.
-ifdef(TEST). 
-include_lib("eunit/include/eunit.hrl"). 
-include("test_constants.hrl").
%All tests based upon ID 9742371 - lolware.net
parse_leaf_test() ->
    ?assertEqual(parse_leaf(?TEST_LEAF_ENTRY), ?TEST_MTL).

mtl_to_subjects_test() ->
    X509 = leaf_parse:xparse(?TEST_MTL),
    ?assertEqual(leaf_parse:get_subjects(X509), ?TEST_DOMAIN_LIST).

-endif.

