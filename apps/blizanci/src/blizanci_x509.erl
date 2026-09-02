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

-export([peercert_cn/1]).
-export([verify_cert/3]).
-export([validate_pem_file/1, certificate_from_file/1]).
-export([check_cert/1]).
-export([verify_signed_by/2]).

%% @doc
%% Check if the connection has a valid certificate,
%% and return the issuer and subject names, or error.
%%
%% A certificate is remote-attacker-controlled input: decoding it, or
%% dumping its RDNs, must never crash the calling process. Any failure
%% is reported as a distinct {error, Reason}, distinguishing a
%% certificate we plain couldn't decode (`cert_not_parsed`) from one
%% which decoded fine but uses a directory-string encoding for an RDN
%% attribute value that we don't know how to render
%% (`cert_unsupported_encoding`), e.g. bmpString/universalString.
%% @end
-spec peercert_cn(C) -> ClientCert
              when C          :: term(),
                   ClientCert :: client_cert().

peercert_cn({ok, Cert}) ->
    try
        Res = public_key:pkix_decode_cert(Cert, otp),
        {Issuer, Subject} = cert_rdns(Res),
        {ok, RDN} = dump_rdn(Subject),
        {ok, RDN_Issuer} = dump_rdn(Issuer),
        CN = proplists:get_value(common_name, RDN),
        IssuerCN = proplists:get_value(common_name, RDN_Issuer),
        Result = #{ common_name => CN, issuer_common_name => IssuerCN },
        {ok, Result}
    catch
        throw:unsupported_rdn_value_encoding ->
            {error, cert_unsupported_encoding};
        _:_ ->
            {error, cert_not_parsed}
    end;
peercert_cn(_) ->
    error.


%% @doc Return a tuple of the Common Names of the issuer and handler of a
%% certificate.
%% @end
-spec cert_rdns(Cert) -> CommonNames
              when Cert        :: #'OTPCertificate'{},
                   CommonNames :: {term(), term()}.

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


-spec dump_rdn(RDN) -> Result
              when RDN    :: term(),
                   Result :: {ok, term()}.

dump_rdn({rdnSequence, Data}) ->
    {ok, [ {oid_alias(Oid), munge_utf8(Value) } ||
        [{'AttributeTypeAndValue', Oid, Value}] <- Data ]
    };
dump_rdn(_X) ->
    throw(rdn_parse_failure).


-spec munge_utf8(String) -> Data
              when String :: list() | {'utf8String', binary()},
                   Data   :: binary().

munge_utf8(S) when is_list(S)                    -> list_to_binary(S);
munge_utf8({utf8String, B}) when is_binary(B)    -> B;
munge_utf8({printableString, S}) when is_list(S) -> list_to_binary(S);
munge_utf8({teletexString, S}) when is_list(S)   -> list_to_binary(S);
munge_utf8({ia5String, S}) when is_list(S)       -> list_to_binary(S);
munge_utf8(_) -> throw(unsupported_rdn_value_encoding).


-spec oid_alias(OID) -> Alias
              when OID   :: tuple(),
                   Alias :: atom().

oid_alias({2,5,4,3}) -> common_name;
oid_alias({2,5,4,6}) -> country;
oid_alias({2,5,4,8}) -> location;
oid_alias({2,5,4,10}) -> organisation;
oid_alias(_) -> unknown.


-spec verify_cert(OtpCert, Event, InitialUserState) -> Result
              when OtpCert          :: #'OTPCertificate'{},
                   Event            :: {'bad_cert',
                                        Reason :: atom() | {'revoked', atom()} } |
                                       {'extension', #'Extension'{}} |
                                       'valid' |
                                       'valid_peer',
                   InitialUserState :: term(),
                   Result           :: {'valid', UserState :: term()} |
                                       {'fail', Reason :: term()} |
                                       {'unknown', UserState :: term()}.

verify_cert(_Cert, _Event, _InitialUserState) ->
    {valid, unknown_user}.


%% @doc
%% Decode a raw client certificate and check that the current time falls
%% within its validity period (NotBefore/NotAfter).
%%
%% As with peercert_cn/1, the input is remote-attacker-controlled, so a
%% certificate we can't decode at all is reported distinctly
%% (`cert_not_parsed`) from one which decodes fine but has expired
%% (`cert_expired`).
%% @end
-spec check_cert(DerCert) -> Result
              when DerCert :: public_key:der_encoded(),
                   Result  :: {ok, #'OTPCertificate'{}}
                            | {error, cert_expired}
                            | {error, cert_not_parsed}.

check_cert(DerCert) ->
    try
        OtpCert = public_key:pkix_decode_cert(DerCert, otp),
        case cert_time_valid(OtpCert) of
            true  -> {ok, OtpCert};
            false -> {error, cert_expired}
        end
    catch
        _:_ -> {error, cert_not_parsed}
    end.


-spec cert_time_valid(OtpCert) -> boolean()
              when OtpCert :: #'OTPCertificate'{}.

cert_time_valid(Cert) ->
    {'OTPCertificate', Data, _, _} = Cert,
    {'OTPTBSCertificate',
     _Version, _Serial, _Signature, _Issuer,
     Validity, _Subject, _PubKey, _X1, _X2, _X3} = Data,
    {'Validity', NotBefore, NotAfter} = Validity,
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    Now >= asn1_time_to_gregorian(NotBefore) andalso
        Now =< asn1_time_to_gregorian(NotAfter).


% UTCTime and GeneralizedTime, per RFC 5280 (PKIX profile), must be
% expressed in UTC (a "Z" suffix) with no fractional seconds; that's
% assumed here, and any certificate which violates it will throw and be
% caught by check_cert/1's try/catch, i.e., be treated as cert_not_parsed.
-spec asn1_time_to_gregorian(Time) -> Seconds
              when Time    :: {'utcTime', string()}
                            | {'generalTime', string()},
                   Seconds :: integer().

asn1_time_to_gregorian({utcTime, [Y1, Y2 | Rest]}) ->
    Year2 = list_to_integer([Y1, Y2]),
    Year = case Year2 >= 50 of
               true  -> 1900 + Year2;
               false -> 2000 + Year2
           end,
    to_gregorian(Year, Rest);
asn1_time_to_gregorian({generalTime, Str}) ->
    {YearStr, Rest} = lists:split(4, Str),
    to_gregorian(list_to_integer(YearStr), Rest).


% Rest is "MoMoDDHHMMSSZ", i.e. month/day/hour/minute/second pairs
% followed by the mandatory "Z" (see asn1_time_to_gregorian/1 above).
-spec to_gregorian(Year, Rest) -> Seconds
              when Year    :: integer(),
                   Rest    :: string(),
                   Seconds :: integer().

to_gregorian(Year, [Mo1,Mo2,D1,D2,H1,H2,Mi1,Mi2,S1,S2,$Z]) ->
    calendar:datetime_to_gregorian_seconds(
      {{Year, list_to_integer([Mo1,Mo2]), list_to_integer([D1,D2])},
       {list_to_integer([H1,H2]), list_to_integer([Mi1,Mi2]),
        list_to_integer([S1,S2])}}).


%% @doc
%% Check that DerCert was actually signed by IssuerCert's private key --
%% a genuine cryptographic check, not the name-comparison that
%% public_key:pkix_is_issuer/2 does (that function only compares the
%% Issuer field of DerCert against the Subject field of IssuerCert; it
%% is meant for candidate-chain construction, not a standalone trust
%% decision, and is trivially spoofable by anyone able to set those
%% fields on a self-signed certificate).
%%
%% Deliberately minimal: this does not require IssuerCert to be a
%% "proper" CA certificate (no basicConstraints/keyUsage checks, unlike
%% public_key:pkix_path_validation/3) -- blizanci's issuer certs are
%% used the way an organisation runs its own internal CA for e.g.
%% mail/VPN clients, not as web-PKI CAs, and may not carry those
%% extensions.
%% @end
-spec verify_signed_by(DerCert, IssuerCert) -> Result
              when DerCert    :: public_key:der_encoded(),
                   IssuerCert :: #'OTPCertificate'{},
                   Result     :: boolean().

verify_signed_by(DerCert, IssuerCert) ->
    try
        Key = issuer_public_key(IssuerCert),
        public_key:pkix_verify(DerCert, Key)
    catch
        _:_ -> false
    end.


-spec issuer_public_key(IssuerCert) -> Key
              when IssuerCert :: #'OTPCertificate'{},
                   Key        :: public_key:public_key().

issuer_public_key(#'OTPCertificate'{tbsCertificate = TBS}) ->
    #'OTPTBSCertificate'{subjectPublicKeyInfo = SPKI} = TBS,
    #'OTPSubjectPublicKeyInfo'{subjectPublicKey = Key} = SPKI,
    Key.


%% @doc
%% Return the decoded certificate from a PEM-encoded file.
%% This returns only the first certificate available in the file, and likely
%% crashes when run on a file not containing any PEM-encoded certificates.
%% @end
-spec certificate_from_file(Path) -> Cert
              when Path :: string(),
                   Cert :: #'Certificate'{} | #'OTPCertificate'{}.

certificate_from_file(Path) ->
    {ok, Data} = file:read_file(Path),
    PEM_Entries = public_key:pem_decode(Data),
    {value, {_, DerCert, _}} = lists:keysearch('Certificate', 1, PEM_Entries),
    public_key:pkix_decode_cert(DerCert, otp).


%% @doc
%% Check that a file containts (at least) one PEM-encoded certificate.
%% @end
-spec validate_pem_file(Filename) -> Result
              when Filename :: string(),
                   Result   :: {ok, string()} | {fail, atom()}.

validate_pem_file(Filename) ->
    case file:read_file(Filename) of
        {ok, PemBin} ->
            case public_key:pem_decode(PemBin) of
                [] ->
                    logger:warning("File ~p not a PEM cert.", [Filename]),
                    {fail, not_a_cert};
                _ -> {ok, Filename}
            end;
        _ ->
            logger:warning("Couldn't open ~p", [Filename]),
            {fail, couldnt_open_pem_file}
    end.
