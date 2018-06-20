-module(leaf_parse).
-export([parse_leaf/1, xparse/1, get_subjects/1, get_serial/1, paddedhex/1]).

-include_lib("public_key/include/public_key.hrl").

%% Extracts an encoded certificate from JSON
-spec parse_leaf(_) -> any().
parse_leaf(RAW) ->
    {JSON} = jiffy:decode(RAW),
    proplists:get_value(<<"entries">>, JSON).

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
    try [Ext || Ext <-
        Cert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.extensions,
        Ext#'Extension'.extnID == {2, 5, 29, 17} ] of
    [] ->
        [];
    [FirstNameExtension| _] ->
        FirstNameExtension#'Extension'.extnValue
    catch % List will crash on certificates with asn1_NOVALUE
    error:function_clause ->
        []
    end.

-spec get_serial(#'OTPCertificate'{tbsCertificate::#'OTPTBSCertificate'{serialNumber::integer()}}) -> {'serial', string()}.
get_serial(Cert) ->
    Serial = Cert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'
        .serialNumber,
    {serial, paddedhex(Serial)}.

% https://stackoverflow.com/questions/45821143/ssl-certificates-leading-zeros-are-displayed-in-windows-but-not-unix-ksh-shell
-spec paddedhex(pos_integer()) -> string().
paddedhex(X) ->
    Hex = integer_to_list(X, 16),
    case hd(Hex) >= $8 of
    true ->
        "00" ++ Hex;
    _ ->
        Hex
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("test_constants.hrl").
%All tests based upon ID 9742371 - lolware.net
parse_leaf_test() ->
    [{LeafTest}] = parse_leaf(?TEST_LEAF_ENTRY),
    LeafTest2 = proplists:get_value(<<"leaf_input">>, LeafTest),
    ?assertEqual(?TEST_MTL, LeafTest2).

paddedhex_test() ->
    ?assertEqual("00ED2DF6455F94C89589E12F4E2F174FA3",
                 paddedhex(315265683328745785102777192423882182563)).

get_serial_test() ->
    X509 = leaf_parse:xparse(?TEST_MTL),
    ?assertEqual({serial, "19F169D2A081E71A79CE2219220D0B582D6"},
        get_serial(X509)).

mtl_to_subjects_test() ->
    X509 = leaf_parse:xparse(?TEST_MTL),
    ?assertEqual(?TEST_DOMAIN_LIST, leaf_parse:get_subjects(X509)).

-endif.

