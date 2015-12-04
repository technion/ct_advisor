-module(leaf_parse).
-compile([debug_info, export_all]).

-include_lib("public_key/include/public_key.hrl").

%% Extracts an encoded certificate from JSON
-spec parse_leaf(_) -> any().
parse_leaf(RAW) ->
    {JSON} = jiffy:decode(RAW),
    [{Entries}] = proplists:get_value(<<"entries">>, JSON),
    proplists:get_value(<<"leaf_input">>, Entries).

%% Decodes the merkle leaf packed structure to return a certificate.
-spec xparse(binary()) -> any().
xparse(MerkleLeafB64) ->
    MerkleLeafBin = base64:decode(MerkleLeafB64),
    % Logtype = 0. Crash on fail.
    <<_Version:8, _LeafType:8, _Timestamp:64, 0:16,
        _ASNLen:24, Cert/binary>> = MerkleLeafBin,
    public_key:pkix_decode_cert(Cert, otp).

    %% Parses the subjectnames from a certificate.
-spec get_subjects(#'OTPCertificate'{tbsCertificate::#'OTPTBSCertificate'{extensions::[any()]}}) -> any().
get_subjects(Cert) ->
    NameExtensions = [Ext || Ext <-
        Cert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.extensions,
        Ext#'Extension'.extnID == {2, 5, 29, 17} ],
    case NameExtensions of
    [] ->
        [];
    [FirstNameExtension| _] ->
        FirstNameExtension#'Extension'.extnValue
    end.

-spec get_serial(#'OTPCertificate'{tbsCertificate::#'OTPTBSCertificate'{serialNumber::integer()}}) -> {'serial',string()}.
get_serial(Cert) ->
    Serial = Cert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'
        .serialNumber,
    {serial, integer_to_list(Serial, 16)}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("test_constants.hrl").
%All tests based upon ID 9742371 - lolware.net
parse_leaf_test() ->
    ?assertEqual(?TEST_MTL, parse_leaf(?TEST_LEAF_ENTRY)).

get_serial_test() ->
    X509 = leaf_parse:xparse(?TEST_MTL),
    ?assertEqual({serial, "19F169D2A081E71A79CE2219220D0B582D6"},
        get_serial(X509)).

mtl_to_subjects_test() ->
    X509 = leaf_parse:xparse(?TEST_MTL),
    ?assertEqual(?TEST_DOMAIN_LIST, leaf_parse:get_subjects(X509)).

-endif.

