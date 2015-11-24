-module(domain_parse).
-compile([debug_info, export_all]).

% A list of certificates with a list of names
cert_domain_list(Domains) ->
    lists:foreach(fun(X) -> per_cert_domains(X) end, Domains).

% A list of domains for an individual certificate
per_cert_domains(Domains) ->
    case lists:flatten(lookup_name_list(Domains)) of
    [] ->
        ok;
    Alerts ->
        io:fwrite("We have an alert for ~p with cert ~p~n", [Alerts, Domains])
    end.

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

    
