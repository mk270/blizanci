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
-include_lib("eunit/include/eunit.hrl").
-include("blizanci_types.hrl").

-export([peercert_cn/1]).
-export([verify_cert/3]).
-export([validate_pem_file/1, certificate_from_file/1]).
-export([check_cert/1]).
-export([verify_signed_by/2]).

% Certificate-making helpers for the tests here and in blizanci_auth.
-ifdef(TEST).
-export([test_pki/0, test_tmp_dir/0, test_write_pem/3]).
-endif.

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
                                        Reason :: atom()
                                                | {'revoked', atom()} }
                                     | {'extension', #'Extension'{}}
                                     | 'valid'
                                     | 'valid_peer',
                   InitialUserState :: term(),
                   Result           :: {'valid', UserState :: term()}
                                     | {'fail', Reason :: term()}
                                     | {'unknown', UserState :: term()}.

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Certificates are made here, with public_key, so that the tests need no
% fixture files and no openssl: each is a v1 certificate carrying only a
% subject, an issuer, a validity period and an RSA key, signed with
% SHA-256. test_pki/0 makes a small cast of them:
%
%   ca, ca2   two unrelated self-signed CAs ("Test CA", "Other CA")
%   peer      "alice", signed by ca
%   peer_ca2  "bob", signed by ca2
%   forged    "alice" again, whose Issuer is ca's name, but which is
%             signed by an attacker's own key (the 2026-09 forgery)
%   expired, not_yet   signed by ca, outside their validity period
%   general, pivot     valid now; times written as GeneralizedTime, and
%                      as UTCTime across the 1950/2049 pivot
%   bmp, printable, no_cn   subjects with a BMPString CN, a
%                      PrintableString CN, and no CN at all
%
% Keys are 2048-bit RSA with exponent 17, as elsewhere in the tests.

-define(DAY, 86400).

test_key() ->
    public_key:generate_key({rsa, 2048, 17}).

test_rsa_public(#'RSAPrivateKey'{modulus = N, publicExponent = E}) ->
    #'RSAPublicKey'{modulus = N, publicExponent = E}.

test_dn(none) ->
    {rdnSequence, []};
test_dn(Value) ->
    {rdnSequence,
     [[#'AttributeTypeAndValue'{type = ?'id-at-commonName',
                                value = Value}]]}.

test_cn(String) ->
    test_dn({utf8String, list_to_binary(String)}).

% A UTCTime for a time given in gregorian seconds.
test_utc(Seconds) ->
    {{Y, Mo, D}, {H, Mi, S}} =
        calendar:gregorian_seconds_to_datetime(Seconds),
    {utcTime, lists:flatten(
                io_lib:format("~2..0w~2..0w~2..0w~2..0w~2..0w~2..0wZ",
                              [Y rem 100, Mo, D, H, Mi, S]))}.

test_cert(Subject, Issuer, SubjectKey, SignKey, {NotBefore, NotAfter}) ->
    Alg = #'SignatureAlgorithm'{algorithm = ?'sha256WithRSAEncryption',
                                parameters = asn1_NOVALUE},
    SPKI = #'OTPSubjectPublicKeyInfo'{
              algorithm = #'PublicKeyAlgorithm'{
                             algorithm = ?rsaEncryption,
                             parameters = asn1_NOVALUE},
              subjectPublicKey = test_rsa_public(SubjectKey)},
    TBS = #'OTPTBSCertificate'{
             version = v1,
             serialNumber = 1,
             signature = Alg,
             issuer = Issuer,
             validity = #'Validity'{notBefore = NotBefore,
                                    notAfter = NotAfter},
             subject = Subject,
             subjectPublicKeyInfo = SPKI,
             issuerUniqueID = asn1_NOVALUE,
             subjectUniqueID = asn1_NOVALUE,
             extensions = asn1_NOVALUE},
    public_key:pkix_sign(TBS, SignKey).

test_pki() ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    Valid = {test_utc(Now - ?DAY), test_utc(Now + ?DAY)},
    CAKey = test_key(),
    CA2Key = test_key(),
    PeerKey = test_key(),
    AttackerKey = test_key(),
    CAName = test_cn("Test CA"),
    CA2Name = test_cn("Other CA"),
    Signed = fun(Subject, Validity) ->
                 test_cert(Subject, CAName, PeerKey, CAKey, Validity)
             end,
    CA = test_cert(CAName, CAName, CAKey, CAKey, Valid),
    CA2 = test_cert(CA2Name, CA2Name, CA2Key, CA2Key, Valid),
    #{ ca       => CA,
       ca_otp   => public_key:pkix_decode_cert(CA, otp),
       ca2      => CA2,
       ca2_otp  => public_key:pkix_decode_cert(CA2, otp),
       peer_key => PeerKey,
       peer     => Signed(test_cn("alice"), Valid),
       peer_ca2 => test_cert(test_cn("bob"), CA2Name, PeerKey, CA2Key,
                             Valid),
       forged   => test_cert(test_cn("alice"), CAName, AttackerKey,
                             AttackerKey, Valid),
       expired  => Signed(test_cn("alice"),
                          {test_utc(Now - 2 * ?DAY), test_utc(Now - ?DAY)}),
       not_yet  => Signed(test_cn("alice"),
                          {test_utc(Now + ?DAY), test_utc(Now + 2 * ?DAY)}),
       general  => Signed(test_cn("alice"),
                          {{generalTime, "20200101000000Z"},
                           {generalTime, "20991231235959Z"}}),
       pivot    => Signed(test_cn("alice"),
                          {{utcTime, "500101000000Z"},
                           {utcTime, "491231235959Z"}}),
       bmp      => Signed(test_dn({bmpString, [0, $a]}), Valid),
       printable => Signed(test_dn({printableString, "carol"}), Valid),
       no_cn    => Signed(test_dn(none), Valid)
     }.

% A fresh, empty directory for a test's files; the caller removes it.
test_tmp_dir() ->
    Base = case os:getenv("TMPDIR") of
               false -> "/tmp";
               Dir   -> Dir
           end,
    Unique = integer_to_list(erlang:unique_integer([positive])),
    Path = filename:join(Base, "blizanci_x509_test_" ++ Unique),
    ok = filelib:ensure_path(Path),
    Path.

% Write a PEM file holding the given entries; returns its path.
test_write_pem(Dir, Name, Entries) ->
    Path = filename:join(Dir, Name),
    ok = file:write_file(Path, public_key:pem_encode(Entries)),
    Path.

test_pem_cert(Der) ->
    {'Certificate', Der, not_encrypted}.

pki_fixture(Tests) ->
    {setup, fun test_pki/0, fun(_) -> ok end, Tests}.

% The cryptographic check: a certificate is accepted as issued by a CA
% only if the CA's key signed it.
verify_signed_by_test_() ->
    pki_fixture(
      fun(#{ca := CA, ca_otp := CAOtp, ca2_otp := CA2Otp, peer := Peer,
            peer_ca2 := PeerCA2, forged := Forged}) ->
          [?_assert(verify_signed_by(Peer, CAOtp)),
           ?_assert(verify_signed_by(PeerCA2, CA2Otp)),
           % a self-signed CA is signed by its own key
           ?_assert(verify_signed_by(CA, CAOtp)),
           % signed by someone else
           ?_assertNot(verify_signed_by(Peer, CA2Otp)),
           ?_assertNot(verify_signed_by(PeerCA2, CAOtp)),
           % The 2026-09 forgery: the Issuer field names the CA, but the
           % signature is the attacker's. The old check, a comparison of
           % names (public_key:pkix_is_issuer/2), accepts it; the
           % signature check rejects it.
           ?_assert(public_key:pkix_is_issuer(Forged, CAOtp)),
           ?_assertNot(verify_signed_by(Forged, CAOtp)),
           % and a genuine certificate passes both
           ?_assert(public_key:pkix_is_issuer(Peer, CAOtp)),
           % anything undecodable is "not signed by", never a crash
           ?_assertNot(verify_signed_by(<<"not a certificate">>, CAOtp)),
           ?_assertNot(verify_signed_by(<<>>, CAOtp)),
           ?_assertNot(apply(?MODULE, verify_signed_by,
                             [Peer, not_a_certificate]))]
      end).

% Whether the certificate is within its validity period.
check_cert_test_() ->
    pki_fixture(
      fun(#{peer := Peer, expired := Expired, not_yet := NotYet,
            general := General, pivot := Pivot}) ->
          [?_assertMatch({ok, #'OTPCertificate'{}}, check_cert(Peer)),
           ?_assertEqual({ok, public_key:pkix_decode_cert(Peer, otp)},
                         check_cert(Peer)),
           ?_assertEqual({error, cert_expired}, check_cert(Expired)),
           % a certificate not yet valid is also reported as cert_expired
           ?_assertEqual({error, cert_expired}, check_cert(NotYet)),
           % GeneralizedTime, and UTCTime either side of the 1950/2049
           % pivot of two-digit years (a valid 1950-2049 period)
           ?_assertMatch({ok, _}, check_cert(General)),
           ?_assertMatch({ok, _}, check_cert(Pivot)),
           % undecodable input is reported, not raised
           ?_assertEqual({error, cert_not_parsed},
                         check_cert(<<"not a certificate">>)),
           ?_assertEqual({error, cert_not_parsed}, check_cert(<<>>)),
           ?_assertEqual({error, cert_not_parsed},
                         check_cert(binary:part(Peer, 0, 100)))]
      end).

% A time that isn't UTC ("Z"), which RFC 5280 forbids, makes the
% certificate unparseable as far as check_cert is concerned.
check_cert_non_utc_time_test_() ->
    pki_fixture(
      fun(#{peer_key := Key}) ->
          Name = test_cn("alice"),
          Cert = fun(NotAfter) ->
                     test_cert(Name, Name, Key, Key,
                               {{utcTime, "200101000000Z"}, NotAfter})
                 end,
          [?_assertMatch({ok, _},
                         check_cert(Cert({utcTime, "491231235959Z"}))),
           ?_assertEqual({error, cert_not_parsed},
                         check_cert(Cert({utcTime, "491231235959+0100"})))]
      end).

% The common names the certificate carries, for the CGI environment.
peercert_cn_test_() ->
    pki_fixture(
      fun(#{peer := Peer, bmp := Bmp, printable := Printable,
            no_cn := NoCN, expired := Expired}) ->
          [?_assertEqual({ok, #{ common_name => <<"alice">>,
                                issuer_common_name => <<"Test CA">> }},
                         peercert_cn({ok, Peer})),
           ?_assertEqual({ok, #{ common_name => <<"carol">>,
                                issuer_common_name => <<"Test CA">> }},
                         peercert_cn({ok, Printable})),
           % No Common Name at all: the name comes back as the atom
           % 'undefined', though the spec (cert_details()) says binary();
           % through apply/3, since the spec otherwise rules this out.
           % (blizanci_cgi then makes it REMOTE_USER=undefined.)
           ?_assertEqual({ok, #{ common_name => undefined,
                                issuer_common_name => <<"Test CA">> }},
                         apply(?MODULE, peercert_cn, [{ok, NoCN}])),
           % an RDN string encoding it doesn't render
           ?_assertEqual({error, cert_unsupported_encoding},
                         peercert_cn({ok, Bmp})),
           % this does not look at the validity period
           ?_assertMatch({ok, #{ common_name := <<"alice">> }},
                         peercert_cn({ok, Expired})),
           ?_assertEqual({error, cert_not_parsed},
                         peercert_cn({ok, <<"not a certificate">>})),
           ?_assertEqual({error, cert_not_parsed}, peercert_cn({ok, <<>>})),
           % no certificate presented: the bare atom, not a tuple
           ?_assertEqual(error, peercert_cn({error, no_peercert})),
           ?_assertEqual(error, peercert_cn(anything_else))]
      end).

% The TLS-level verify_fun accepts every certificate and every event,
% bad ones included: whether a client certificate is acceptable is
% decided later, per route, by blizanci_auth. (A self-signed client
% certificate must get as far as that decision.)
verify_cert_test_() ->
    pki_fixture(
      fun(#{ca_otp := CAOtp}) ->
          Events = [valid,
                    valid_peer,
                    {bad_cert, unknown_ca},
                    {bad_cert, cert_expired},
                    {bad_cert, selfsigned_peer},
                    {bad_cert, {revoked, unspecified}},
                    {extension, #'Extension'{extnID = {2,5,29,19},
                                             critical = true,
                                             extnValue = <<>>}}],
          [ ?_assertEqual({valid, unknown_user},
                          verify_cert(CAOtp, Event, [])) || Event <- Events ]
      end).

% PEM files: the server's own, and the CA files of a `private` route.
pem_files_test_() ->
    {setup,
     fun() ->
         Pki = test_pki(),
         Dir = test_tmp_dir(),
         #{ca := CA, ca2 := CA2, peer_key := Key} = Pki,
         Files = #{
           ca      => test_write_pem(Dir, "ca.pem", [test_pem_cert(CA)]),
           both    => test_write_pem(Dir, "both.pem",
                                     [test_pem_cert(CA), test_pem_cert(CA2)]),
           key     => test_write_pem(Dir, "key.pem",
                                     [public_key:pem_entry_encode(
                                        'RSAPrivateKey', Key)]),
           empty   => test_write_pem(Dir, "empty.pem", []),
           text    => filename:join(Dir, "text.pem"),
           missing => filename:join(Dir, "missing.pem")},
         ok = file:write_file(maps:get(text, Files), <<"hello\n">>),
         {Dir, Pki, Files}
     end,
     fun({Dir, _, _}) -> file:del_dir_r(Dir) end,
     fun({_, #{ca_otp := CAOtp}, Files}) ->
         #{ca := CAFile, both := Both, key := KeyFile, empty := Empty,
           text := Text, missing := Missing} = Files,
         [?_assertEqual({ok, CAFile}, validate_pem_file(CAFile)),
          ?_assertEqual({ok, Both}, validate_pem_file(Both)),
          % any PEM entry will do: it need not be a certificate
          ?_assertEqual({ok, KeyFile}, validate_pem_file(KeyFile)),
          ?_assertEqual({fail, not_a_cert}, validate_pem_file(Empty)),
          ?_assertEqual({fail, not_a_cert}, validate_pem_file(Text)),
          ?_assertEqual({fail, couldnt_open_pem_file},
                        validate_pem_file(Missing)),
          ?_assertEqual(CAOtp, certificate_from_file(CAFile)),
          % only the first certificate is returned
          ?_assertEqual(CAOtp, certificate_from_file(Both)),
          % a file with no certificate in it, or no file, raises
          ?_assertError({badmatch, false}, certificate_from_file(KeyFile)),
          ?_assertError({badmatch, false}, certificate_from_file(Empty)),
          ?_assertError({badmatch, false}, certificate_from_file(Text)),
          ?_assertError({badmatch, {error, enoent}},
                        certificate_from_file(Missing))]
     end}.
