-module(leaf_iterate).
-compile([debug_info, export_all]).

get_domain_from_id(ID) ->
    LeafEntry =  ct_fetch:fetch_entry(ID),
    MTL = leaf_parse:parse_leaf(LeafEntry),
    X509 = leaf_parse:xparse(MTL),
    leaf_parse:get_subjects(X509).

enumerate_ids(ID, ID) ->
    get_domain_from_id(ID);

enumerate_ids(FROM, TO) when FROM < TO ->
    [get_domain_from_id(FROM)| enumerate_ids(FROM+1, TO)].

