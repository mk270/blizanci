%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_auth).

-moduledoc """
The authorisation policies of a route (public, restricted, private)
and the checking of a client certificate against them.

See the manual chapter [Authorisation](authorisation.md).
""".
-include("blizanci_types.hrl").

-export([authorisation_policy/1, authorised/2, valid_authz_policy/2]).
-export([realm/1, add_realm/3, realm_table/1]).

-ifdef(TEST).
%% Exported for the tests in test/blizanci_auth_tests.erl.
-export([valid_authz_policy/1]).
-endif.

-define(MAX_REALM_NAME, 64).

-doc """
Derive the authorisation policy of a given routing table entry

Returns `{ok, public | restricted | private} | {error, atom()}`.
""".
-spec authorisation_policy(Options) -> Result
              when Options :: map(),
                   Result  :: {'ok', authorisation()} | {'error', term()}.

authorisation_policy(Options) when is_map(Options) ->
    #{ authorisation := Auth } = Options,
    valid_authz_policy(Auth).


-doc """
Check if the argument represents a valid authorisation policy
""".
-spec valid_authz_policy(Policy) -> Result
              when Policy :: term(),
                   Result :: {ok, authorisation()} | {error, term()}.
valid_authz_policy(Policy) ->
    valid_authz_policy(Policy, #{}).


-doc """
As valid_authz_policy/1, resolving any realms against a realm
table (see realm_table/1).
""".
-spec valid_authz_policy(Policy, Realms) -> Result
              when Policy :: term(),
                   Realms :: realm_table(),
                   Result :: {ok, authorisation()} | {error, term()}.
% For {private, Files} the files are read and every certificate in them
% decoded here; the result is {private, Files, Certs}, Certs being all
% the certificates of all the files, in order.  A file that is missing,
% unreadable, holds no certificate or holds any undecodable certificate
% is refused with {invalid_ca_file, File, Why}.
%
% For {private, Realm} or {private, [Realm]} the realms are looked up in
% the table, once, here; the result is {private, Realms, Certs}, Certs
% being the certificates of all the named realms.  A realm that is not
% in the table is {unknown_realm, Realm}; one with no certificates
% is fine and authorises nobody.  A realm tuple that
% realm/1 would not have built is {invalid_realm_name, Name}.

valid_authz_policy(public, _) -> {ok, public};
valid_authz_policy(restricted, _) -> {ok, restricted};
valid_authz_policy({private, {realm, _} = Realm}, Realms) ->
    resolve_realms([Realm], Realms, []);
valid_authz_policy({private, [{realm, _} | _] = List}, Realms) ->
    resolve_realms(List, Realms, []);
valid_authz_policy({private, Files}, _) when is_list(Files) ->
    case load_ca_files(Files, []) of
        {ok, Certs} -> {ok, {private, Files, Certs}};
        {error, _} = Error -> Error
    end;
valid_authz_policy(_, _) -> {error, invalid_authz_policy}.


-spec resolve_realms(List, Realms, Acc) -> Result
              when List   :: [term()],
                   Realms :: realm_table(),
                   Acc    :: [{realm(), [#'OTPCertificate'{}]}],
                   Result :: {ok, authorisation()} | {error, term()}.
resolve_realms([], _, Acc) ->
    Found = lists:reverse(Acc),
    {ok, {private, [ R || {R, _} <- Found ],
          lists:append([ C || {_, C} <- Found ])}};
resolve_realms([{realm, Name} = Realm | Rest], Realms, Acc) ->
    case realm(Name) of
        {ok, Realm} ->
            case maps:find(Realm, Realms) of
                {ok, {_, Certs}} ->
                    resolve_realms(Rest, Realms, [{Realm, Certs} | Acc]);
                error -> {error, {unknown_realm, Realm}}
            end;
        {error, _} -> {error, {invalid_realm_name, Name}}
    end;
resolve_realms(_, _, _) ->
    {error, invalid_authz_policy}.


-doc """
The one way to make a realm: a non-empty, case-sensitive name of
printable ASCII, at most 64 characters.
""".
-spec realm(Name) -> Result
              when Name   :: term(),
                   Result :: {ok, realm()} | {error, term()}.
realm(Name) when is_binary(Name), byte_size(Name) > 0,
                 byte_size(Name) =< ?MAX_REALM_NAME ->
    case lists:all(fun(C) -> C >= $\s andalso C =< $~ end,
                   binary_to_list(Name)) of
        true  -> {ok, {realm, Name}};
        false -> {error, {invalid_realm_name, Name}}
    end;
realm(Name) ->
    {error, {invalid_realm_name, Name}}.


-doc """
Add a realm to a realm table, reading and decoding its CA files
now.  A realm may have no files; a private route on it
authorises nobody.
""".
-spec add_realm(Realms, Name, Files) -> Result
              when Realms :: realm_table(),
                   Name   :: term(),
                   Files  :: term(),
                   Result :: {ok, realm_table()} | {error, term()}.
add_realm(Realms, Name, Files) ->
    case realm(Name) of
        {error, _} = Error -> Error;
        {ok, Realm} ->
            case maps:is_key(Realm, Realms) of
                true -> {error, {duplicate_realm, Realm}};
                false when is_list(Files) ->
                    case load_ca_files(Files, []) of
                        {ok, Certs} ->
                            {ok, Realms#{Realm => {Files, Certs}}};
                        {error, _} = Error -> Error
                    end;
                false -> {error, {invalid_realm_files, Realm}}
            end
    end.


-doc """
Build a realm table from a list of {Name, Files}, as in the
sys.config key realms.
""".
-spec realm_table(Defs) -> Result
              when Defs   :: term(),
                   Result :: {ok, realm_table()} | {error, term()}.
realm_table(Defs) when is_list(Defs) ->
    lists:foldl(fun({Name, Files}, {ok, T}) -> add_realm(T, Name, Files);
                   (Other, {ok, _}) -> {error, {invalid_realm, Other}};
                   (_, Error) -> Error
                end, {ok, #{}}, Defs);
realm_table(Defs) ->
    {error, {invalid_realms, Defs}}.


-spec load_ca_files(Files, Acc) -> Result
              when Files  :: [term()],
                   Acc    :: [#'OTPCertificate'{}],
                   Result :: {'ok', [#'OTPCertificate'{}]}
                           | {'error', {'invalid_ca_file', term(),
                                        term()}}.
load_ca_files([], Acc) ->
    {ok, Acc};
load_ca_files([File | Rest], Acc) ->
    case blizanci_x509:certificates_from_file(File) of
        {ok, Certs} -> load_ca_files(Rest, Acc ++ Certs);
        {error, Why} -> {error, {invalid_ca_file, File, Why}}
    end.


-doc """
Check if a route's authorisation policy permits a given request

- `AuthPolicy`: the authorisation policy associated with the route
- `Request`: the Gemini request
""".
-spec authorised(AuthPolicy, Request) -> Result
              when AuthPolicy :: authorisation(),
                   Request    :: request_details(),
                   Result     :: 'authorised' | {'error_code', atom()}.

authorised(public, _Request) -> authorised;
authorised(AuthPolicy, Request) ->
    #{ client_cert := Cert } = Request,
    cert_authorised(AuthPolicy, Cert).


-spec cert_authorised(Authorisation, CertInfo) -> Result
              when Authorisation :: authorisation(),
                   CertInfo      :: term(),
                   Result        :: 'authorised' | {'error_code', atom()}.

cert_authorised(_, {error, no_peercert}) ->
    {error_code, cert_required};
cert_authorised(AuthPolicy, {ok, DerCert}) ->
    case blizanci_x509:check_cert(DerCert) of
        {ok, _OtpCert} ->
            cert_authorised_policy(AuthPolicy, DerCert);
        {error, cert_expired} ->
            {error_code, cert_expired};
        {error, cert_not_yet_valid} ->
            {error_code, cert_not_yet_valid};
        {error, cert_not_parsed} ->
            {error_code, cert_not_parsed}
    end.


-doc """
Check a certificate, already known to be within its validity
period, against the policy's issuer requirements (if any). DerCert
is the raw certificate (not the decoded #'OTPCertificate'{} form),
since that's what public_key:pkix_verify/2 (via
blizanci_x509:verify_signed_by/2) requires.
""".
-spec cert_authorised_policy(Authorisation, DerCert) -> Result
              when Authorisation :: authorisation(),
                   DerCert       :: public_key:der_encoded(),
                   Result        :: 'authorised'
                                  | {'error_code', atom()}.

cert_authorised_policy(restricted, _DerCert) ->
    authorised;
cert_authorised_policy({private, _Files, Issuers}, DerCert) ->
    case cert_issued_by_any(DerCert, Issuers) of
        true  -> authorised;
        false -> {error_code, cert_not_authorised}
    end.


-doc """
Check that DerCert was cryptographically signed by the private
key of one of the issuer certificates -- see
blizanci_x509:verify_signed_by/2 for why this isn't just
public_key:pkix_is_issuer/2 (a name comparison, not a signature
check).
""".
-spec cert_issued_by_any(DerCert, Issuers) -> Result
              when DerCert :: public_key:der_encoded(),
                   Issuers :: [#'OTPCertificate'{}],
                   Result  :: boolean().

cert_issued_by_any(DerCert, Issuers) ->
    lists:any(fun(Issuer) ->
                  blizanci_x509:verify_signed_by(DerCert, Issuer)
              end, Issuers).
