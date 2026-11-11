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
-include_lib("eunit/include/eunit.hrl").
-include("blizanci_types.hrl").

-export([authorisation_policy/1, authorised/2, valid_authz_policy/1]).

%% @doc Derive the authorisation policy of a given routing table entry
%% @returns {ok, public | restricted | private} | {error, atom()}
%% @end
-spec authorisation_policy(Options) -> Result
              when Options :: map(),
                   Result  :: {'ok', authorisation()} | {'error', term()}.

authorisation_policy(Options) when is_map(Options) ->
    #{ authorisation := Auth } = Options,
    valid_authz_policy(Auth).


%% @doc Check if the argument represents a valid authorisation policy
%% @end
-spec valid_authz_policy(Policy) -> Result
              when Policy :: term(),
                   Result :: {ok, authorisation()} | {error, term()}.

valid_authz_policy(public) -> {ok, public};
valid_authz_policy(restricted) -> {ok, restricted};
valid_authz_policy({private, Files}) when is_list(Files) ->
    % the issuer files are read at request time, so a bad one is
    % refused here, when routes are built, rather than crash a request
    case valid_ca_files(Files) of
        ok -> {ok, {private, Files}};
        {error, _} = Error -> Error
    end;
valid_authz_policy(_) -> {error, invalid_authz_policy}.


-spec valid_ca_files(Files) -> Result
              when Files  :: [term()],
                   Result :: 'ok' | {'error', {'invalid_ca_file', term(),
                                              term()}}.
valid_ca_files([]) ->
    ok;
valid_ca_files([File | Rest]) ->
    case blizanci_x509:validate_ca_file(File) of
        ok -> valid_ca_files(Rest);
        {error, Why} -> {error, {invalid_ca_file, File, Why}}
    end.


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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The certificates come from blizanci_x509:test_pki/0 (see the comment
% there): a CA ("Test CA", file ca.pem) and an unrelated CA ("Other CA",
% ca2.pem); a client certificate signed by each; a forgery naming the
% first CA as its issuer but signed by someone else; and expired and
% not-yet-valid certificates signed by the first CA.

policy_test_() ->
    [?_assertEqual({ok, public}, valid_authz_policy(public)),
     ?_assertEqual({ok, restricted}, valid_authz_policy(restricted)),
     ?_assertEqual({ok, {private, []}}, valid_authz_policy({private, []})),
     ?_assertEqual({error, invalid_authz_policy},
                   valid_authz_policy(private)),
     ?_assertEqual({error, invalid_authz_policy},
                   valid_authz_policy(secret)),
     ?_assertEqual({error, invalid_authz_policy},
                   valid_authz_policy({restricted, []})),
     % the CA files of a private policy must be a list
     ?_assertEqual({error, invalid_authz_policy},
                   apply(?MODULE, valid_authz_policy,
                         [{private, not_a_list}]))].

% The CA files of a private policy are checked when the policy is
% validated (that is, when the route is built), not on the first request.
policy_private_ca_files_test_() ->
    authorised_fixture(
      fun(_Pki, #{ca := CA, ca2 := CA2, text := Text, missing := Missing,
                  dir := Dir}) ->
          Valid = fun(Files) -> valid_authz_policy({private, Files}) end,
          Bad = fun(File, Why) ->
                    {error, {invalid_ca_file, File, Why}}
                end,
          Write = fun(Name, Data) ->
                      File = filename:join(Dir, Name),
                      ok = file:write_file(File, Data),
                      File
                  end,
          Key = Write("key.pem", <<"-----BEGIN PRIVATE KEY-----\n"
                                   "AQID\n"
                                   "-----END PRIVATE KEY-----\n">>),
          Junk = Write("junk.pem", <<"-----BEGIN CERTIFICATE-----\n"
                                     "AQID\n"
                                     "-----END CERTIFICATE-----\n">>),
          Empty = Write("empty.pem", <<>>),
          [?_assertEqual({ok, {private, [CA]}}, Valid([CA])),
           ?_assertEqual({ok, {private, [CA, CA2]}}, Valid([CA, CA2])),
           ?_assertEqual({ok, {private, []}}, Valid([])),
           ?_assertEqual(Bad(Missing, enoent), Valid([Missing])),
           ?_assertEqual(Bad(Dir, eisdir), Valid([Dir])),
           ?_assertEqual(Bad(Text, no_certificate_in_file), Valid([Text])),
           ?_assertEqual(Bad(Empty, no_certificate_in_file),
                         Valid([Empty])),
           ?_assertEqual(Bad(Key, no_certificate_in_file), Valid([Key])),
           ?_assertEqual(Bad(Junk, certificate_not_decodable),
                         Valid([Junk])),
           % the first bad file is the one reported, wherever it is
           ?_assertEqual(Bad(Missing, enoent), Valid([CA, Missing, Text])),
           % a CA must be a string, not a binary or anything else
           ?_assertEqual(Bad(list_to_binary(CA), not_a_path),
                         apply(?MODULE, valid_authz_policy,
                               [{private, [list_to_binary(CA)]}])),
           ?_assertEqual(Bad("", not_a_path), Valid([""]))]
      end).

authorisation_policy_test_() ->
    [?_assertEqual({ok, restricted},
                   authorisation_policy(#{ authorisation => restricted })),
     ?_assertEqual({ok, {private, []}},
                   authorisation_policy(
                     #{ authorisation => {private, []},
                        other => option })),
     ?_assertEqual({error, invalid_authz_policy},
                   authorisation_policy(#{ authorisation => nonsense })),
     % the option is required
     ?_assertError({badmatch, _},
                   apply(?MODULE, authorisation_policy, [#{}]))].

% A complete request, as the router would pass it, carrying the client's
% certificate (or the lack of one).
request(ClientCert) ->
    #{ client_cert   => ClientCert,
       query         => <<>>,
       rest_of_input => <<>>,
       scheme        => <<"gemini">>,
       host          => <<"h">>,
       port          => 1965,
       path          => <<"/x">> }.

authorised_fixture(Tests) ->
    {setup,
     fun() ->
         Pki = blizanci_x509:test_pki(),
         Dir = blizanci_x509:test_tmp_dir(),
         Pem = fun(Name, Der) ->
                   blizanci_x509:test_write_pem(
                     Dir, Name, [{'Certificate', Der, not_encrypted}])
               end,
         #{ca := CA, ca2 := CA2} = Pki,
         Text = filename:join(Dir, "text.pem"),
         ok = file:write_file(Text, <<"hello\n">>),
         Files = #{ca      => Pem("ca.pem", CA),
                   ca2     => Pem("ca2.pem", CA2),
                   text    => Text,
                   missing => filename:join(Dir, "missing.pem"),
                   dir     => Dir},
         {Dir, Pki, Files}
     end,
     fun({Dir, _, _}) -> file:del_dir_r(Dir) end,
     fun({_, Pki, Files}) -> Tests(Pki, Files) end}.

% A public route needs no certificate, and does not even look at one.
authorised_public_test_() ->
    authorised_fixture(
      fun(#{peer := Peer}, _Files) ->
          [?_assertEqual(authorised,
                         authorised(public, request({error, no_peercert}))),
           ?_assertEqual(authorised,
                         authorised(public, request({ok, Peer}))),
           ?_assertEqual(authorised,
                         authorised(public, request({ok, <<"garbage">>})))]
      end).

% A restricted route needs a certificate that is within its validity
% period; it does not matter who issued it.
authorised_restricted_test_() ->
    authorised_fixture(
      fun(#{peer := Peer, peer_ca2 := PeerCA2, forged := Forged,
            expired := Expired, not_yet := NotYet}, _Files) ->
          Auth = fun(Cert) -> authorised(restricted, request(Cert)) end,
          [?_assertEqual({error_code, cert_required},
                         Auth({error, no_peercert})),
           ?_assertEqual(authorised, Auth({ok, Peer})),
           ?_assertEqual(authorised, Auth({ok, PeerCA2})),
           % even a forged certificate will do: the issuer is not asked
           ?_assertEqual(authorised, Auth({ok, Forged})),
           ?_assertEqual({error_code, cert_expired}, Auth({ok, Expired})),
           ?_assertEqual({error_code, cert_expired}, Auth({ok, NotYet})),
           ?_assertEqual({error_code, cert_not_parsed},
                         Auth({ok, <<"garbage">>})),
           ?_assertEqual({error_code, cert_not_parsed}, Auth({ok, <<>>}))]
      end).

% A private route needs a certificate signed by one of its CAs.
authorised_private_test_() ->
    authorised_fixture(
      fun(#{peer := Peer, peer_ca2 := PeerCA2, forged := Forged,
            expired := Expired, not_yet := NotYet},
          #{ca := CAFile, ca2 := CA2File}) ->
          Auth = fun(CAs, Cert) ->
                     authorised({private, CAs}, request(Cert))
                 end,
          NoCert = {error, no_peercert},
          [% signed by the one CA
           ?_assertEqual(authorised, Auth([CAFile], {ok, Peer})),
           % signed by a different CA
           ?_assertEqual({error_code, cert_not_authorised},
                         Auth([CAFile], {ok, PeerCA2})),
           ?_assertEqual({error_code, cert_not_authorised},
                         Auth([CA2File], {ok, Peer})),
           % The 2026-09 forgery: its Issuer is the CA's name, but it was
           % not signed by the CA's key.
           ?_assertEqual({error_code, cert_not_authorised},
                         Auth([CAFile], {ok, Forged})),
           ?_assertEqual({error_code, cert_not_authorised},
                         Auth([CAFile, CA2File], {ok, Forged})),
           % any one of several CAs will do, in either order
           ?_assertEqual(authorised, Auth([CAFile, CA2File], {ok, Peer})),
           ?_assertEqual(authorised, Auth([CA2File, CAFile], {ok, Peer})),
           ?_assertEqual(authorised,
                         Auth([CAFile, CA2File], {ok, PeerCA2})),
           % no CAs, so nobody is let in
           ?_assertEqual({error_code, cert_not_authorised},
                         Auth([], {ok, Peer})),
           % the earlier checks come first
           ?_assertEqual({error_code, cert_required},
                         Auth([CAFile], NoCert)),
           ?_assertEqual({error_code, cert_expired},
                         Auth([CAFile], {ok, Expired})),
           ?_assertEqual({error_code, cert_expired},
                         Auth([CAFile], {ok, NotYet})),
           ?_assertEqual({error_code, cert_not_parsed},
                         Auth([CAFile], {ok, <<"garbage">>}))]
      end).

% What happens when a private route's CA file cannot be used by
% authorised/2 itself, i.e. if the file goes bad after the route was
% built (building the route refuses it, see policy_private_ca_files_test_).
% Nothing catches the failure: it raises, on the first request that
% carries a (valid) certificate. A CA that comes after the one that
% matches is never read.
authorised_private_bad_ca_file_test_() ->
    authorised_fixture(
      fun(#{peer := Peer}, #{ca := CAFile, text := Text,
                             missing := Missing}) ->
          Auth = fun(CAs) ->
                     authorised({private, CAs}, request({ok, Peer}))
                 end,
          [?_assertError({badmatch, {error, enoent}}, Auth([Missing])),
           ?_assertError({badmatch, false}, Auth([Text])),
           % reached only if the earlier CAs did not match
           ?_assertError({badmatch, {error, enoent}},
                         Auth(["/nonexistent", CAFile])),
           ?_assertEqual(authorised, Auth([CAFile, Missing])),
           % a CA given as a binary rather than a string
           ?_assertError(function_clause,
                         apply(?MODULE, authorised,
                               [{private, [list_to_binary(CAFile)]},
                                request({ok, Peer})]))]
      end).
