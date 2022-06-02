%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

%% @doc
%% This module handles most of the certificate-related functionality.
%%
%% Herein, RDN means "relative distinguished name".
%% @end

-module(blizanci_x509).
-include_lib("public_key/include/public_key.hrl").
-include("blizanci_types.hrl").

-export([cert_rdns/1, dump_rdn/1, report_peercert/1]).
-export([peercert_cn/1]).
-export([verify_cert/3]).
-export([certificate_from_file/1]).

-spec peercert_cn(term()) -> client_cert().
peercert_cn({ok, Cert}) ->
    Res = public_key:pkix_decode_cert(Cert, otp),
    {Issuer, Subject} = cert_rdns(Res),
    {ok, RDN} = dump_rdn(Subject),
    {ok, RDN_Issuer} = dump_rdn(Issuer),
    Result =
        #{
          common_name => proplists:get_value(common_name, RDN),
          issuer_common_name => proplists:get_value(common_name, RDN_Issuer)
         },
    {ok, Result};
peercert_cn(_) ->
    error.

-spec report_peercert(term()) -> 'ok'.
report_peercert({ok, Cert}) ->
    Res = public_key:pkix_decode_cert(Cert, otp),
    {_Issuer, Subject} = blizanci_x509:cert_rdns(Res),
    lager:info("RDN: ~p", [blizanci_x509:dump_rdn(Subject)]),
    ok;

report_peercert({error, no_peercert}) ->
    lager:info("No peer cert"),
    ok;

report_peercert(_) ->
    lager:info("No peer cert, unknown error"),
    ok.

%% TBD: types
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
     _X1,
     _X2,
     _X3} = Data,
    {IssuerRDN, SubjectRDN}.

%% TBD: types
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


-spec verify_cert(
        OtpCert :: #'OTPCertificate'{},
        Event :: {'bad_cert', Reason :: atom() | {'revoked', atom()}} |
                 {'extension', #'Extension'{}} |
                 'valid' |
                 'valid_peer',
        InitialUserState :: term()
       ) -> {'valid', UserState :: term()} |
            {'fail', Reason :: term()} |
            {'unknown', UserState :: term()}.
verify_cert(_Cert, _Event, _InitialUserState) ->
    {valid, unknown_user}.


-spec certificate_from_file(string()) -> #'Certificate'{}.
certificate_from_file(Path) ->
    {ok, Data} = file:read_file(Path),
    PEM_Entries = public_key:pem_decode(Data),
    {value, {_, DerCert, _}} = lists:keysearch('Certificate', 1, PEM_Entries),
    public_key:pkix_decode_cert(DerCert, otp).
