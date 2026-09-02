%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2022  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

% TBD
% private: certificate must be signed by a particular CA
% restricted: certificate must be presented
% public: no certificate requirement

-module(blizanci_auth).
-include_lib("public_key/include/public_key.hrl").
-include("blizanci_types.hrl").

-export([authorisation_policy/1, authorised/2, valid_authz_policy/1]).

%% @doc Derive the authorisation policy of a given routing table entry
%% @returns {ok, public | restricted | private} | {error, atom()}
%% @end
-spec authorisation_policy(Options) -> Result
              when Options :: map(),
                   Result  :: {'ok', authorisation()} | {'error', atom()}.

authorisation_policy(Options) when is_map(Options) ->
    #{ authorisation := Auth } = Options,
    valid_authz_policy(Auth).


%% @doc Check if the argument represents a valid authorisation policy
%% @end
-spec valid_authz_policy(Policy) -> Result
              when Policy :: term(),
                   Result :: {ok, authorisation()} | {error, atom()}.

valid_authz_policy(public) -> {ok, public};
valid_authz_policy(restricted) -> {ok, restricted};
valid_authz_policy({private, Keys}) -> {ok, {private, Keys}};
valid_authz_policy(_) -> {error, invalid_authz_policy}.


%% @doc Check if a route's authorisation policy permits a given request
%% @param AuthPolicy the authorisation policy associated with the route
%% @param Request the Gemini request
%% @end
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
        {error, cert_not_parsed} ->
            {error_code, cert_not_parsed}
    end.


%% @doc Check a certificate, already known to be within its validity
%% period, against the policy's issuer requirements (if any). DerCert
%% is the raw certificate (not the decoded #'OTPCertificate'{} form),
%% since that's what public_key:pkix_verify/2 (via
%% blizanci_x509:verify_signed_by/2) requires.
%% @end
-spec cert_authorised_policy(Authorisation, DerCert) -> Result
              when Authorisation :: authorisation(),
                   DerCert       :: public_key:der_encoded(),
                   Result        :: 'authorised'
                                  | {'error_code', atom()}.

cert_authorised_policy(restricted, _DerCert) ->
    authorised;
cert_authorised_policy({private, Certs}, DerCert) ->
    case cert_issued_by_any(DerCert, Certs) of
        {ok, _Issuer} -> authorised;
        fail -> {error_code, cert_not_authorised}
    end.


-spec cert_issued_by_any(DerCert, Issuers) -> Result
              when DerCert :: public_key:der_encoded(),
                   Issuers :: [string()],
                   Result  :: {'ok', string()} | 'fail'.

cert_issued_by_any(_DerCert, []) -> fail;
cert_issued_by_any(DerCert, [Issuer|Tail]) when is_list(Issuer) ->
    case cert_issued_by(DerCert, Issuer) of
        ok -> {ok, Issuer};
        not_issuer -> cert_issued_by_any(DerCert, Tail)
    end.


%% @doc Check that DerCert was cryptographically signed by the private
%% key of the issuer certificate at path Issuer -- see
%% blizanci_x509:verify_signed_by/2 for why this isn't just
%% public_key:pkix_is_issuer/2 (a name comparison, not a signature
%% check).
%% @end
-spec cert_issued_by(DerCert, Issuer) -> Result
              when DerCert :: public_key:der_encoded(),
                   Issuer  :: string(),
                   Result  :: 'ok' | 'not_issuer'.

cert_issued_by(DerCert, Issuer) ->
    IssuerCert = blizanci_x509:certificate_from_file(Issuer),
    case blizanci_x509:verify_signed_by(DerCert, IssuerCert) of
        true  -> ok;
        false -> not_issuer
    end.
