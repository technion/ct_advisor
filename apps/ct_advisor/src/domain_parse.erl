-module(domain_parse).
-compile([debug_info, export_all]).

% A list of certificates with a list of names
-spec cert_domain_list([any()]) -> 'ok'.
cert_domain_list(Domains) ->
    lists:foreach(fun(X) -> per_cert_domains(X) end, Domains).

% A list of domains for an individual certificate
-spec per_cert_domains([{'dNSName',_}]) -> 'ok'.
per_cert_domains(Domains) ->
    case lists:flatten(lookup_name_list(Domains)) of
    [] ->
        ok;
    Alerts ->
        io:fwrite("We have an alert for ~p with cert ~p~n", [Alerts, Domains])
    end.

-spec lookup_name_list([{'dNSName',_}]) -> [[] | {_,_}].
lookup_name_list([]) ->
    [];

lookup_name_list([{dNSName, Name}|Tail]) ->
    %Match = "Name " ++ Name ++ "is matching",
    Match = case ets:lookup(users, Name) of
    [{Name, User}] ->
        {Name, User};
    _ ->
        []
    end,
    [Match|lookup_name_list(Tail)].

    
