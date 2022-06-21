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
-include("blizanci_types.hrl").

-export([authorisation_policy/1, authorised/2, valid_authz_policy/1]).

-spec authorisation_policy(map()) ->
          {'ok', authorisation()} | {'error', atom()}.
%% @doc Derive the authorisation policy of a given routing table entry
%% @returns {ok, public | restricted | private} | {error, atom()}
%% @end
authorisation_policy(Options) when is_map(Options) ->
    #{ authorisation := Auth } = Options,
    valid_authz_policy(Auth).

-spec valid_authz_policy(term()) -> {ok, authorisation()} | {error, atom()}.
%% @doc Check if the argument represents a valid authorisation policy
%% @end
valid_authz_policy(public) -> {ok, public};
valid_authz_policy(restricted) -> {ok, restricted};
valid_authz_policy({private, Keys}) -> {ok, {private, Keys}};
valid_authz_policy(_) -> {error, invalid_authz_policy}.

-spec authorised(AuthPolicy::authorisation(), Request::map()) ->
          'authorised' | {'error_code', atom()}.
%% @doc Check if a route's authorisation policy permits a given request
%% @param AuthPolicy the authorisation policy associated with the route
%% @param Request the Gemini request
%% @end
authorised(public, _Request) -> authorised;
authorised(AuthPolicy, Request) ->
    #{ client_cert := Cert } = Request,
    cert_authorised(AuthPolicy, Cert).

%% TODO: should check for expiry
-spec cert_authorised(authorisation(), term()) ->
          'authorised' | {'error_code', atom()}.
cert_authorised(_, {error, no_peercert}) ->
    {error_code, cert_required};
cert_authorised(restricted, {ok, _AnyCert}) ->
    authorised;
cert_authorised({private, Certs}, {ok, Cert}) when is_map(Cert) ->
    case cert_issued_by_any(Cert, Certs) of
        {ok, Issuer} -> lager:debug("successful auth: ~p", [Issuer]),
                        authorised;
        fail -> {error_code, cert_not_authorised}
    end.


-spec cert_issued_by_any(map(), [string()]) ->
          {'ok', string()} | 'fail'.
cert_issued_by_any(Cert, []) when is_map(Cert) -> fail;
cert_issued_by_any(Cert, [Issuer|Tail]) when is_list(Issuer) and
                                             is_map(Cert) ->
    case cert_issued_by(Cert, Issuer) of
        ok -> {ok, Issuer};
        not_issuer -> cert_issued_by_any(Cert, Tail)
    end.

-spec cert_issued_by(map(), Issuer::string()) ->
          'ok' | 'not_issuer'.
cert_issued_by(Cert, Issuer) when is_map(Cert) ->
    IssuerCert = blizanci_x509:certificate_from_file(Issuer),
    case public_key:pkix_is_issuer(Cert, IssuerCert) of
        false -> not_issuer;
        true -> ok
    end.
