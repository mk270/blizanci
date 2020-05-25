%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_x509).

-export([cert_rdns/1, dump_rdn/1]).

cert_rdns(Cert) ->
    {'OTPCertificate', Data, _, _} = Cert,
    {'OTPTBSCertificate',
     _Version,
     _Serial,
     _Signature,
     IssuerRDN,
     _Validity,
     SubjectRDN,
     _PubKey,
     _,
     _,
     _} = Data,
    {IssuerRDN, SubjectRDN}.

dump_rdn({rdnSequence, Data}) ->
    {ok, [ {oid_alias(Oid), munge_utf8(Value) } ||
        [{'AttributeTypeAndValue', Oid, Value}] <- Data ]
    };
dump_rdn(_X) ->
    {error, rdn_parse_failure}.

munge_utf8(S) when is_list(S)                 -> list_to_binary(S);
munge_utf8({utf8String, B}) when is_binary(B) -> B.

oid_alias({2,5,4,3}) -> common_name;
oid_alias({2,5,4,6}) -> country;
oid_alias({2,5,4,8}) -> location;
oid_alias({2,5,4,10}) -> organisation;
oid_alias(_) -> unknown.
