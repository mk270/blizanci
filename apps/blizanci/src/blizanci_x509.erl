%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_x509).

-moduledoc """
This module handles most of the certificate-related functionality.

Herein, RDN means "relative distinguished name".
""".
-include("blizanci_types.hrl").

-export([peercert_cn/1]).
-export([verify_cert/3]).
-export([validate_pem_file/1, certificates_from_file/1]).
-export([check_cert/1]).
-export([cert_key_match/2]).
-export([verify_signed_by/2]).

% RFC 5280 UTCTime has a two-digit year: 50 to 99 mean 1950 to 1999 and
% 00 to 49 mean 2000 to 2049.
-define(UTC_PIVOT_YEAR, 50).
-define(UTC_CENTURY_HIGH, 1900).
-define(UTC_CENTURY_LOW, 2000).

% Certificate-making helpers for the tests here and in blizanci_auth.
-ifdef(TEST).
-export([test_pki/0, test_tmp_dir/0, test_write_pem/3]).
-export([test_pair_files/3]).
%% Also used by the tests in test/blizanci_x509_tests.erl.
-export([test_cert/5, test_cn/1, test_pem_cert/1, test_utc/1]).
-endif.

-doc """
Check if the connection has a valid certificate,
and return the issuer and subject names, or error.

A certificate is remote-attacker-controlled input: decoding it, or
dumping its RDNs, must never crash the calling process. Any failure
is reported as a distinct {error, Reason}, distinguishing a
certificate we plain couldn't decode (`cert_not_parsed`) from one
which decoded fine but uses a directory-string encoding for an RDN
attribute value that we don't know how to render
(`cert_unsupported_encoding`), e.g. bmpString/universalString.

A certificate with no Common Name is not an error: the corresponding
value in the result is the atom `undefined` (for the subject, the
issuer, or both).
""".
-spec peercert_cn(C) -> ClientCert
              when C          :: term(),
                   ClientCert :: client_cert_details().

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


-doc """
Return a tuple of the Common Names of the issuer and handler of a
certificate.
""".
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


-doc """
Decode a raw client certificate and check that the current time falls
within its validity period (NotBefore/NotAfter).

As with peercert_cn/1, the input is remote-attacker-controlled, so a
certificate we can't decode at all is reported distinctly
(`cert_not_parsed`) from one which decodes fine but is outside its
validity period: past NotAfter (`cert_expired`) or before NotBefore
(`cert_not_yet_valid`).
""".
-spec check_cert(DerCert) -> Result
              when DerCert :: public_key:der_encoded(),
                   Result  :: {ok, #'OTPCertificate'{}}
                            | {error, cert_expired}
                            | {error, cert_not_yet_valid}
                            | {error, cert_not_parsed}.

check_cert(DerCert) ->
    try
        OtpCert = public_key:pkix_decode_cert(DerCert, otp),
        case cert_time_status(OtpCert) of
            valid         -> {ok, OtpCert};
            expired       -> {error, cert_expired};
            not_yet_valid -> {error, cert_not_yet_valid}
        end
    catch
        _:_ -> {error, cert_not_parsed}
    end.


-spec cert_time_status(OtpCert) -> Status
              when OtpCert :: #'OTPCertificate'{},
                   Status  :: 'valid' | 'expired' | 'not_yet_valid'.

cert_time_status(Cert) ->
    {'OTPCertificate', Data, _, _} = Cert,
    {'OTPTBSCertificate',
     _Version, _Serial, _Signature, _Issuer,
     Validity, _Subject, _PubKey, _X1, _X2, _X3} = Data,
    {'Validity', NotBefore, NotAfter} = Validity,
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    case Now < asn1_time_to_gregorian(NotBefore) of
        true  -> not_yet_valid;
        false ->
            case Now > asn1_time_to_gregorian(NotAfter) of
                true  -> expired;
                false -> valid
            end
    end.


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
    Year = case Year2 >= ?UTC_PIVOT_YEAR of
               true  -> ?UTC_CENTURY_HIGH + Year2;
               false -> ?UTC_CENTURY_LOW + Year2
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


-doc """
Check that DerCert was actually signed by IssuerCert's private key --
a genuine cryptographic check, not the name-comparison that
public_key:pkix_is_issuer/2 does (that function only compares the
Issuer field of DerCert against the Subject field of IssuerCert; it
is meant for candidate-chain construction, not a standalone trust
decision, and is trivially spoofable by anyone able to set those
fields on a self-signed certificate).

Deliberately minimal: this does not require IssuerCert to be a
"proper" CA certificate (no basicConstraints/keyUsage checks, unlike
public_key:pkix_path_validation/3) -- blizanci's issuer certs are
used the way an organisation runs its own internal CA for e.g.
mail/VPN clients, not as web-PKI CAs, and may not carry those
extensions.
""".
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


-doc """
Return every certificate in a PEM-encoded file, decoded, in file order
(a CA bundle gives all of its CAs). Other PEM entries (keys, say) are
ignored. Does not raise. The error is a posix atom if the file cannot
be read, 'not_a_path' if Path is not a non-empty string,
'no_certificate_in_file' if there is no certificate entry, and
'certificate_not_decodable' if ANY certificate entry fails to decode
(the file is refused whole, not partly used).
""".
-spec certificates_from_file(Path) -> Result
              when Path   :: term(),
                   Result :: {'ok', [#'OTPCertificate'{}, ...]}
                           | {'error', term()}.

certificates_from_file(Path) when is_list(Path), Path =/= [] ->
    try file:read_file(Path) of
        {ok, Data} -> certificates_from_pem(Data);
        {error, Posix} -> {error, Posix}
    catch
        _:_ -> {error, not_a_path}
    end;
certificates_from_file(_) ->
    {error, not_a_path}.

-spec certificates_from_pem(Data) -> Result
              when Data   :: binary(),
                   Result :: {'ok', [#'OTPCertificate'{}, ...]}
                           | {'error', term()}.
certificates_from_pem(Data) ->
    try [Der || {'Certificate', Der, _} <- public_key:pem_decode(Data)] of
        [] -> {error, no_certificate_in_file};
        Ders -> decode_all(Ders, [])
    catch
        _:_ -> {error, no_certificate_in_file}
    end.

-spec decode_all(Ders, Acc) -> Result
              when Ders   :: [binary()],
                   Acc    :: [#'OTPCertificate'{}],
                   Result :: {'ok', [#'OTPCertificate'{}, ...]}
                           | {'error', 'certificate_not_decodable'}.
decode_all([], Acc) ->
    {ok, lists:reverse(Acc)};
decode_all([Der | Rest], Acc) ->
    try public_key:pkix_decode_cert(Der, otp) of
        #'OTPCertificate'{} = Cert -> decode_all(Rest, [Cert | Acc]);
        _ -> {error, certificate_not_decodable}
    catch
        _:_ -> {error, certificate_not_decodable}
    end.


-doc """
Check that a file containts (at least) one PEM-encoded certificate.
""".
-spec validate_pem_file(Filename) -> Result
              when Filename :: string(),
                   Result   :: {ok, string()} | {error, atom()}.

validate_pem_file(Filename) ->
    case file:read_file(Filename) of
        {ok, PemBin} ->
            case public_key:pem_decode(PemBin) of
                [] ->
                    logger:warning("File ~p not a PEM cert.", [Filename]),
                    {error, not_a_cert};
                _ -> {ok, Filename}
            end;
        _ ->
            logger:warning("Couldn't open ~p", [Filename]),
            {error, couldnt_open_pem_file}
    end.


-doc """
Check that a PEM certificate file and a PEM private key file are a
matching pair, so that a server using them can complete a handshake.
The certificate is the first one in its file (the leaf, when the file
holds a chain); the key is the first private key in its file, which
may be the same file. RSA keys match on modulus and public exponent;
EC keys (P-256, P-384, P-521) on the public point derived from the
private scalar, which must be on the same named curve; Ed25519 and
Ed448 likewise. Nothing is trusted from the file's own idea of the
public key. PKCS#1, PKCS#8 and SEC1 containers are all handled.

Result is `ok` for a verified match; `{ok, unverified, Reason}` for a
pair the check cannot compare, where Reason is
`{unsupported_key_type, Name}` (Name a string such as "DSA" or
"EC secp224r1"), which is NOT a failure and the caller should warn;
`{error, mismatch}` for a pair that provably does not match; or
`{error, {certfile, Why}}` / `{error, {keyfile, Why}}` for a file
problem, which names the file at fault: `{unreadable, Posix}`,
`no_certificate_in_file` or `certificate_not_decodable` (certfile),
`{unreadable, Posix}`, `no_private_key`, `encrypted_private_key` or
`private_key_not_decodable` (keyfile).

The rule: each side's family is RSA, EC (any curve, Ed25519 and
Ed448 included) or DSA when we can tell. Two different families are a
mismatch, even if one is a family we cannot compare, so an RSA
certificate with a DSA key is refused. Within RSA, and within EC on
the supported curves, the keys are compared. Two EC keys on
different named curves are a mismatch. Otherwise (DSA with DSA, the
same unsupported EC curve, RSA-PSS or any other algorithm we do not
know, on either side) the pair is unverified.
""".
-spec cert_key_match(CertFile, KeyFile) -> Result
              when CertFile :: string(),
                   KeyFile  :: string(),
                   Result   :: 'ok'
                             | {'ok', 'unverified', Reason}
                             | {'error', term()},
                   Reason   :: {'unsupported_key_type', string()}.

cert_key_match(CertFile, KeyFile) ->
    try
        Pub = case cert_public_key(CertFile) of
                  {ok, P} -> P;
                  {error, CWhy} -> throw({error, {certfile, CWhy}})
              end,
        Priv = case private_key_from_file(KeyFile) of
                   {ok, K} -> K;
                   {error, KWhy} -> throw({error, {keyfile, KWhy}})
               end,
        key_matches(Pub, Priv)
    catch
        throw:{error, _} = Error -> Error;
        _:_ -> {error, {keyfile, private_key_not_decodable}}
    end.


-spec read_pem(File) -> Result
              when File   :: term(),
                   Result :: {'ok', [term()]} | {'error', term()}.
read_pem(File) ->
    try file:read_file(File) of
        {ok, Bin} -> {ok, public_key:pem_decode(Bin)};
        {error, Posix} -> {error, {unreadable, Posix}}
    catch
        _:_ -> {error, {unreadable, badarg}}
    end.


% The first certificate's public key, as {Algorithm, Parameters, Key}.
-spec cert_public_key(File) -> Result
              when File   :: term(),
                   Result :: {'ok', {term(), term(), term()}}
                           | {'error', term()}.
cert_public_key(File) ->
    case read_pem(File) of
        {error, _} = Error -> Error;
        {ok, Entries} ->
            case [ D || {'Certificate', D, not_encrypted} <- Entries ] of
                [] -> {error, no_certificate_in_file};
                [Der | _] -> decode_cert_key(Der)
            end
    end.


-spec decode_cert_key(Der) -> Result
              when Der    :: binary(),
                   Result :: {'ok', {term(), term(), term()}}
                           | {'error', 'certificate_not_decodable'}.
decode_cert_key(Der) ->
    try public_key:pkix_decode_cert(Der, otp) of
        #'OTPCertificate'{tbsCertificate = TBS} ->
            #'OTPTBSCertificate'{subjectPublicKeyInfo = SPKI} = TBS,
            #'OTPSubjectPublicKeyInfo'{algorithm = A,
                                       subjectPublicKey = Key} = SPKI,
            #'PublicKeyAlgorithm'{algorithm = Alg, parameters = P} = A,
            {ok, {Alg, P, Key}}
    catch
        _:_ -> {error, certificate_not_decodable}
    end.


-define(PRIVATE_KEY_TYPES, ['RSAPrivateKey', 'ECPrivateKey',
                            'PrivateKeyInfo', 'DSAPrivateKey']).

-spec private_key_from_file(File) -> Result
              when File   :: term(),
                   Result :: {'ok', term()} | {'error', term()}.
private_key_from_file(File) ->
    case read_pem(File) of
        {error, _} = Error -> Error;
        {ok, Entries} ->
            case [ E || {T, _, _} = E <- Entries,
                        lists:member(T, ?PRIVATE_KEY_TYPES) ] of
                [] -> {error, no_private_key};
                [{_, _, not_encrypted} = E | _] ->
                    try {ok, public_key:pem_entry_decode(E)}
                    catch _:_ -> {error, private_key_not_decodable}
                    end;
                [_ | _] ->
                    {error, encrypted_private_key}
            end
    end.


-spec key_matches(CertKey, PrivKey) -> Result
              when CertKey :: {term(), term(), term()},
                   PrivKey :: term(),
                   Result  :: 'ok' | {'ok', 'unverified', term()}
                            | {'error', 'mismatch'}.
key_matches({Alg, Params, Pub} = CertKey, Priv) ->
    CertFamily = cert_family(CertKey),
    case {CertFamily, key_family(Priv)} of
        {rsa, rsa} -> rsa_matches(Pub, Priv);
        {ec, ec} -> ec_matches(Alg, Params, Pub, Priv);
        {dsa, dsa} -> {ok, unverified, {unsupported_key_type, "DSA"}};
        {F, F} -> {ok, unverified, unsupported(CertKey, Priv)};
        {F1, F2} when is_atom(F1), is_atom(F2) -> {error, mismatch};
        _ -> {ok, unverified, unsupported(CertKey, Priv)}
    end.


% The family of a key we can name: rsa, ec (including the Edwards
% curves) or dsa; anything else is {other, _}, which we cannot compare.
-spec cert_family(CertKey) -> Result
              when CertKey :: {term(), term(), term()},
                   Result  :: atom() | {'other', term()}.
cert_family({?rsaEncryption, _, #'RSAPublicKey'{}}) -> rsa;
cert_family({?'id-dsa', _, _}) -> dsa;
cert_family({_, _, #'ECPoint'{}}) -> ec;
cert_family({Alg, _, _}) -> {other, Alg}.


-spec key_family(PrivKey) -> Result
              when PrivKey :: term(),
                   Result  :: atom() | {'other', term()}.
key_family(#'RSAPrivateKey'{}) -> rsa;
key_family(#'ECPrivateKey'{privateKey = S}) when is_binary(S) -> ec;
key_family(#'DSAPrivateKey'{}) -> dsa;
key_family(Other) -> {other, key_kind(Other)}.


% The reason a pair cannot be compared: the certificate's key type if
% that is the one we do not know, else the private key's.
-spec unsupported(CertKey, PrivKey) -> Reason
              when CertKey :: {term(), term(), term()},
                   PrivKey :: term(),
                   Reason  :: {'unsupported_key_type', string()}.
unsupported({Alg, _, _} = CertKey, Priv) ->
    case cert_family(CertKey) of
        {other, _} -> {unsupported_key_type, oid_label(Alg)};
        _ ->
            {other, Kind} = key_family(Priv),
            {unsupported_key_type, oid_label(Kind)}
    end.


-spec oid_label(Alg) -> Label
              when Alg   :: term(),
                   Label :: string().
oid_label(?'id-dsa') -> "DSA";
oid_label({1,2,840,113549,1,1,10}) -> "RSA-PSS";
oid_label(Oid) when is_tuple(Oid), tuple_size(Oid) > 1 ->
    Dotted = lists:join(".", [ integer_to_list(N)
                               || N <- tuple_to_list(Oid),
                                  is_integer(N) ]),
    lists:flatten(Dotted);
oid_label(Other) when is_atom(Other) -> atom_to_list(Other);
oid_label(Other) -> lists:flatten(io_lib:format("~p", [Other])).


-spec rsa_matches(CertKey, PrivKey) -> Result
              when CertKey :: term(),
                   PrivKey :: term(),
                   Result  :: 'ok' | {'error', 'mismatch'}.
rsa_matches(#'RSAPublicKey'{modulus = N, publicExponent = E},
            #'RSAPrivateKey'{modulus = N, publicExponent = E}) ->
    ok;
rsa_matches(_, _) ->
    {error, mismatch}.


% Both are elliptic curve keys. Named curves we support are compared by
% deriving the public point. Two different named curves are a mismatch
% whether we support them or not; the same named curve we do not
% support, or curves not given by name, cannot be verified.
-spec ec_matches(Alg, Params, CertKey, PrivKey) -> Result
              when Alg     :: term(),
                   Params  :: term(),
                   CertKey :: term(),
                   PrivKey :: term(),
                   Result  :: 'ok' | {'ok', 'unverified', term()}
                            | {'error', 'mismatch'}.
ec_matches(Alg, Params, #'ECPoint'{point = Point},
           #'ECPrivateKey'{privateKey = Scalar, parameters = KeyCurve}) ->
    CertCurve = case Alg of
                    ?'id-ecPublicKey' -> Params;
                    _ -> {namedCurve, Alg}
                end,
    case {curve_name(CertCurve), curve_name(KeyCurve)} of
        {{ok, Curve}, {ok, Curve}} ->
            {Derived, _} = crypto:generate_key(curve_kind(Curve), Curve,
                                               Scalar),
            case Derived =:= Point of
                true -> ok;
                false -> {error, mismatch}
            end;
        {{ok, _}, {ok, _}} ->
            {error, mismatch};
        _ ->
            case {CertCurve, KeyCurve} of
                {{namedCurve, Same}, {namedCurve, Same}} ->
                    {ok, unverified, {unsupported_key_type,
                                      curve_label(Same)}};
                {{namedCurve, _}, {namedCurve, _}} ->
                    {error, mismatch};
                {{namedCurve, Oid}, _} ->
                    {ok, unverified, {unsupported_key_type,
                                      curve_label(Oid)}};
                _ ->
                    {ok, unverified, {unsupported_key_type,
                                      "EC (explicit parameters)"}}
            end
    end.


-spec curve_label(Oid) -> Label
              when Oid   :: term(),
                   Label :: string().
curve_label(Oid) ->
    try pubkey_cert_records:namedCurves(Oid) of
        Name when is_atom(Name) -> "EC " ++ atom_to_list(Name);
        _ -> "EC " ++ oid_label(Oid)
    catch
        _:_ -> "EC " ++ oid_label(Oid)
    end.


-spec key_kind(Key) -> Result
              when Key    :: term(),
                   Result :: atom().
key_kind(Key) when is_tuple(Key) -> element(1, Key);
key_kind(_) -> unknown.


-spec curve_name(Params) -> Result
              when Params :: term(),
                   Result :: {'ok', atom()} | 'error'.
curve_name({namedCurve, ?'secp256r1'}) -> {ok, secp256r1};
curve_name({namedCurve, ?'secp384r1'}) -> {ok, secp384r1};
curve_name({namedCurve, ?'secp521r1'}) -> {ok, secp521r1};
curve_name({namedCurve, {1,3,101,112}}) -> {ok, ed25519};
curve_name({namedCurve, {1,3,101,113}}) -> {ok, ed448};
curve_name(_) -> error.


-spec curve_kind(Curve) -> Result
              when Curve  :: atom(),
                   Result :: 'ecdh' | 'eddsa'.
curve_kind(ed25519) -> eddsa;
curve_kind(ed448) -> eddsa;
curve_kind(_) -> ecdh.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test fixtures, used by the tests in test/ and in other modules.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

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
%   no_cn_issuer       "alice", signed by a CA whose name has no CN
%   no_cn_both         no CN, signed by a CA whose name has no CN
%   peer_key           the key of every certificate signed by ca
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

% A certificate and key for a site, written as Name.crt and Name.key in
% Dir; returns their paths. Kind is the key's type: rsa, p256, p384,
% ed25519, ed448, or p224 or dsa (which the pair check cannot verify).
% The certificate is self-signed in form only: it is
% signed by a throwaway RSA key, which is all that matching needs.
test_pair_files(Dir, Name, Kind) ->
    {KeyEntry, SPKI} = test_pair_key(Kind),
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    Subject = test_cn(Name),
    Alg = #'SignatureAlgorithm'{algorithm = ?'sha256WithRSAEncryption',
                                parameters = asn1_NOVALUE},
    TBS = #'OTPTBSCertificate'{
             version = v1, serialNumber = 1, signature = Alg,
             issuer = Subject,
             validity = #'Validity'{notBefore = test_utc(Now - ?DAY),
                                    notAfter = test_utc(Now + ?DAY)},
             subject = Subject, subjectPublicKeyInfo = SPKI,
             issuerUniqueID = asn1_NOVALUE,
             subjectUniqueID = asn1_NOVALUE,
             extensions = asn1_NOVALUE},
    Der = public_key:pkix_sign(TBS, test_key()),
    {test_write_pem(Dir, Name ++ ".crt", [test_pem_cert(Der)]),
     test_write_pem(Dir, Name ++ ".key", [KeyEntry])}.

test_pair_key(rsa) ->
    Key = test_key(),
    {public_key:pem_entry_encode('RSAPrivateKey', Key),
     test_spki(?rsaEncryption, asn1_NOVALUE, test_rsa_public(Key))};
test_pair_key(Curve) when Curve =:= p256; Curve =:= p384 ->
    Oid = case Curve of
              p256 -> ?'secp256r1';
              p384 -> ?'secp384r1'
          end,
    Key = public_key:generate_key({namedCurve, Oid}),
    {public_key:pem_entry_encode('ECPrivateKey', Key),
     test_spki(?'id-ecPublicKey', {namedCurve, Oid},
               #'ECPoint'{point = Key#'ECPrivateKey'.publicKey})};
test_pair_key(p224) ->
    Oid = ?'secp224r1',
    Key = public_key:generate_key({namedCurve, Oid}),
    {public_key:pem_entry_encode('ECPrivateKey', Key),
     test_spki(?'id-ecPublicKey', {namedCurve, Oid},
               #'ECPoint'{point = Key#'ECPrivateKey'.publicKey})};
% not a usable DSA key, but a well-formed one, which is enough here
test_pair_key(dsa) ->
    {P, Q, G, X} = {23, 11, 4, 3},
    Y = 18,
    Key = #'DSAPrivateKey'{version = 0, p = P, q = Q, g = G, y = Y,
                           x = X},
    {public_key:pem_entry_encode('DSAPrivateKey', Key),
     test_spki(?'id-dsa', {params, #'Dss-Parms'{p = P, q = Q, g = G}},
               Y)};
test_pair_key(Kind) when Kind =:= ed25519; Kind =:= ed448 ->
    Oid = case Kind of
              ed25519 -> {1,3,101,112};
              ed448 -> {1,3,101,113}
          end,
    {Pub, Priv} = crypto:generate_key(eddsa, Kind),
    Key = #'ECPrivateKey'{version = 1, privateKey = Priv,
                          parameters = {namedCurve, Oid}},
    {public_key:pem_entry_encode('PrivateKeyInfo', Key),
     test_spki(Oid, asn1_NOVALUE, #'ECPoint'{point = Pub})}.

test_spki(Alg, Params, Key) ->
    #'OTPSubjectPublicKeyInfo'{
       algorithm = #'PublicKeyAlgorithm'{algorithm = Alg,
                                         parameters = Params},
       subjectPublicKey = Key}.

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
    NoCNName = test_dn(none),
    NoCNSigned = fun(Subject, Validity) ->
                     test_cert(Subject, NoCNName, PeerKey, CAKey, Validity)
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
       no_cn    => Signed(test_dn(none), Valid),
       no_cn_issuer => NoCNSigned(test_cn("alice"), Valid),
       no_cn_both   => NoCNSigned(test_dn(none), Valid)
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

-endif.
