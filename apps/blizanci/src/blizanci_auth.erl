%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

% The authorisation policies of a route are:
%
% private: certificate must be signed by one of the listed CAs
% restricted: certificate must be presented
% public: no certificate requirement
%
% The CA files of a private route are read ONCE, when the route is built
% (valid_authz_policy/1), and every certificate found in each file is
% decoded and kept in the route's policy: {private, Files, Certs}.  A
% request is checked against those decoded certificates and does no file
% I/O, so a CA file that is later deleted, damaged or made unreadable
% does not affect a running route.  The consequence is that a changed CA
% file takes effect only when the route is rebuilt (blizanci_config:
% apply/1) or the server is restarted.
%
% A CA file may be a bundle: the client certificate is accepted if it is
% signed by ANY certificate in ANY of the route's files, so trust extends
% to every certificate in a file, not just the first.
%
% A route may instead name authentication realms: {private, Realm} or
% {private, [Realm]}.  A realm is a named set of CA files, defined once
% for the whole server (see add_realm/3) and decoded then.  The route's
% realms are resolved when it is built, and it holds the certificates;
% nothing is looked up per request.

-module(blizanci_auth).
-include_lib("eunit/include/eunit.hrl").
-include("blizanci_types.hrl").

-export([authorisation_policy/1, authorised/2, valid_authz_policy/1,
         valid_authz_policy/2]).
-export([realm/1, add_realm/3, realm_table/1]).

-define(MAX_REALM_NAME, 64).

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
valid_authz_policy(Policy) ->
    valid_authz_policy(Policy, #{}).


%% @doc As valid_authz_policy/1, resolving any realms against a realm
%% table (see realm_table/1).
%% @end
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


%% @doc The one way to make a realm: a non-empty, case-sensitive name of
%% printable ASCII, at most 64 characters.
%% @end
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


%% @doc Add a realm to a realm table, reading and decoding its CA files
%% now.  A realm may have no files; a private route on it
%% authorises nobody.
%% @end
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


%% @doc Build a realm table from a list of {Name, Files}, as in the
%% sys.config key realms.
%% @end
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
        {error, cert_not_yet_valid} ->
            {error_code, cert_not_yet_valid};
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
cert_authorised_policy({private, _Files, Issuers}, DerCert) ->
    case cert_issued_by_any(DerCert, Issuers) of
        true  -> authorised;
        false -> {error_code, cert_not_authorised}
    end.


%% @doc Check that DerCert was cryptographically signed by the private
%% key of one of the issuer certificates -- see
%% blizanci_x509:verify_signed_by/2 for why this isn't just
%% public_key:pkix_is_issuer/2 (a name comparison, not a signature
%% check).
%% @end
-spec cert_issued_by_any(DerCert, Issuers) -> Result
              when DerCert :: public_key:der_encoded(),
                   Issuers :: [#'OTPCertificate'{}],
                   Result  :: boolean().

cert_issued_by_any(DerCert, Issuers) ->
    lists:any(fun(Issuer) ->
                  blizanci_x509:verify_signed_by(DerCert, Issuer)
              end, Issuers).


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
     ?_assertMatch({ok, {private, [], []}},
                   valid_authz_policy({private, []})),
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
      fun(#{ca := CADer}, #{ca := CA, ca2 := CA2, text := Text,
                            missing := Missing, dir := Dir,
                            bundle := Bundle}) ->
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
          BadBundle = blizanci_x509:test_write_pem(
                        Dir, "badbundle.pem",
                        [{'Certificate', CADer, not_encrypted},
                         {'Certificate', <<1, 2, 3>>, not_encrypted}]),
          [?_assertMatch({ok, {private, [CA], [_]}}, Valid([CA])),
           ?_assertMatch({ok, {private, [CA, CA2], [_, _]}},
                         Valid([CA, CA2])),
           % every certificate of a bundle is kept, in file order
           ?_assertMatch({ok, {private, [Bundle], [_, _]}},
                         Valid([Bundle])),
           ?_assertMatch({ok, {private, [Bundle, CA], [_, _, _]}},
                         Valid([Bundle, CA])),
           ?_assertMatch({ok, {private, [], []}}, Valid([])),
           % one undecodable entry refuses a bundle
           ?_assertEqual(Bad(BadBundle, certificate_not_decodable),
                         Valid([BadBundle])),
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
     ?_assertMatch({ok, {private, [], []}},
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
                   bundle  => blizanci_x509:test_write_pem(
                                Dir, "bundle.pem",
                                [{'Certificate', CA, not_encrypted},
                                 {'Certificate', CA2, not_encrypted}]),
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
            expired := Expired, not_yet := NotYet, no_cn := NoCN,
            no_cn_both := NoCNBoth}, _Files) ->
          Auth = fun(Cert) -> authorised(restricted, request(Cert)) end,
          [?_assertEqual({error_code, cert_required},
                         Auth({error, no_peercert})),
           ?_assertEqual(authorised, Auth({ok, Peer})),
           ?_assertEqual(authorised, Auth({ok, PeerCA2})),
           % even a forged certificate will do: the issuer is not asked
           ?_assertEqual(authorised, Auth({ok, Forged})),
           ?_assertEqual({error_code, cert_expired}, Auth({ok, Expired})),
           ?_assertEqual({error_code, cert_not_yet_valid},
                         Auth({ok, NotYet})),
           % a certificate with no Common Name is still accepted
           ?_assertEqual(authorised, Auth({ok, NoCN})),
           ?_assertEqual(authorised, Auth({ok, NoCNBoth})),
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
                     authorised(private_policy(CAs), request(Cert))
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
           ?_assertEqual({error_code, cert_not_yet_valid},
                         Auth([CAFile], {ok, NotYet})),
           ?_assertEqual({error_code, cert_not_parsed},
                         Auth([CAFile], {ok, <<"garbage">>}))]
      end).

% The policy as the route builder makes it, from CA files that are good.
private_policy(Files) ->
    {ok, Policy} = valid_authz_policy({private, Files}),
    Policy.

% A CA bundle: a client signed by any certificate in the file is let in,
% and one signed by neither is not.
authorised_private_bundle_test_() ->
    authorised_fixture(
      fun(#{peer := Peer, peer_ca2 := PeerCA2, forged := Forged},
          #{bundle := Bundle, ca := CAFile}) ->
          Auth = fun(Files, Cert) ->
                     authorised(private_policy(Files), request({ok, Cert}))
                 end,
          [?_assertEqual(authorised, Auth([Bundle], Peer)),
           ?_assertEqual(authorised, Auth([Bundle], PeerCA2)),
           ?_assertEqual({error_code, cert_not_authorised},
                         Auth([Bundle], Forged)),
           % the other CA is not in this single-CA file
           ?_assertEqual({error_code, cert_not_authorised},
                         Auth([CAFile], PeerCA2))]
      end).

% The CA files are read when the route is built and never again: once
% the policy exists, deleting or corrupting the files changes nothing,
% and nothing raises.
authorised_private_no_file_reads_test_() ->
    authorised_fixture(
      fun(#{peer := Peer, peer_ca2 := PeerCA2, forged := Forged},
          #{bundle := Bundle, ca := CAFile, dir := Dir}) ->
          Copy = filename:join(Dir, "copy.pem"),
          {ok, _} = file:copy(Bundle, Copy),
          Policy = private_policy([Copy, CAFile]),
          Auth = fun(Cert) -> authorised(Policy, request({ok, Cert})) end,
          Before = Auth(Peer),
          ok = file:write_file(Copy, <<"corrupted\n">>),
          Corrupted = {Auth(Peer), Auth(PeerCA2), Auth(Forged)},
          ok = file:delete(Copy),
          ok = file:delete(CAFile),
          Deleted = {Auth(Peer), Auth(PeerCA2), Auth(Forged)},
          Expected = {authorised, authorised,
                      {error_code, cert_not_authorised}},
          [?_assertEqual(authorised, Before),
           ?_assertEqual(Expected, Corrupted),
           ?_assertEqual(Expected, Deleted)]
      end).

% The smart constructor accepts only sensible names.
realm_constructor_test_() ->
    Bad = fun(N) -> {error, {invalid_realm_name, N}} end,
    Long = binary:copy(<<"a">>, 64),
    TooLong = binary:copy(<<"a">>, 65),
    [?_assertEqual({ok, {realm, <<"staff">>}}, realm(<<"staff">>)),
     ?_assertEqual({ok, {realm, <<"Staff 1">>}}, realm(<<"Staff 1">>)),
     ?_assertNotEqual(realm(<<"staff">>), realm(<<"Staff">>)),
     ?_assertEqual({ok, {realm, Long}}, realm(Long)),
     ?_assertEqual(Bad(TooLong), realm(TooLong)),
     ?_assertEqual(Bad(<<>>), realm(<<>>)),
     ?_assertEqual(Bad(<<"a\nb">>), realm(<<"a\nb">>)),
     ?_assertEqual(Bad(<<"caf", 195, 169>>), realm(<<"caf", 195, 169>>)),
     ?_assertEqual(Bad(<<127>>), realm(<<127>>)),
     ?_assertEqual(Bad(staff), realm(staff)),
     ?_assertEqual(Bad("staff"), realm("staff"))].

realm_table_test_() ->
    authorised_fixture(
      fun(_Pki, #{ca := CA, ca2 := CA2, text := Text, missing := Missing,
                  bundle := Bundle}) ->
          Staff = {realm, <<"staff">>},
          {ok, T} = realm_table([{<<"staff">>, [CA, CA2]},
                                 {<<"b">>, [Bundle]},
                                 {<<"none">>, []}]),
          [?_assertMatch(#{Staff := {[CA, CA2], [_, _]}}, T),
           ?_assertMatch(#{{realm, <<"b">>} := {[Bundle], [_, _]}}, T),
           ?_assertMatch(#{{realm, <<"none">>} := {[], []}}, T),
           ?_assertEqual({error, {duplicate_realm, Staff}},
                         realm_table([{<<"staff">>, []},
                                      {<<"staff">>, []}])),
           ?_assertEqual({error, {invalid_realm_name, <<>>}},
                         realm_table([{<<>>, []}])),
           ?_assertEqual({error, {invalid_realm_name, staff}},
                         realm_table([{staff, []}])),
           ?_assertEqual({error, {invalid_realm_files, Staff}},
                         realm_table([{<<"staff">>, not_a_list}])),
           ?_assertEqual({error, {invalid_realm, bogus}},
                         realm_table([bogus])),
           ?_assertEqual({error, {invalid_realms, bogus}},
                         realm_table(bogus)),
           ?_assertEqual({error, {invalid_ca_file, Missing, enoent}},
                         realm_table([{<<"x">>, [Missing]}])),
           ?_assertEqual({error, {invalid_ca_file, Text,
                                  no_certificate_in_file}},
                         realm_table([{<<"x">>, [CA, Text]}]))]
      end).

policy_realm_test_() ->
    authorised_fixture(
      fun(#{peer := Peer, peer_ca2 := PeerCA2, forged := Forged},
          #{ca := CA, ca2 := CA2}) ->
          Staff = {realm, <<"staff">>},
          Ops = {realm, <<"ops">>},
          None = {realm, <<"none">>},
          {ok, T} = realm_table([{<<"staff">>, [CA]},
                                 {<<"ops">>, [CA2]},
                                 {<<"none">>, []}]),
          V = fun(P) -> valid_authz_policy(P, T) end,
          Auth = fun(P, Cert) ->
                     {ok, Policy} = V(P),
                     authorised(Policy, request({ok, Cert}))
                 end,
          [?_assertMatch({ok, {private, [Staff], [_]}},
                         V({private, Staff})),
           ?_assertMatch({ok, {private, [Staff, Ops], [_, _]}},
                         V({private, [Staff, Ops]})),
           ?_assertEqual({error, {unknown_realm, {realm, <<"x">>}}},
                         V({private, {realm, <<"x">>}})),
           ?_assertEqual({error, {unknown_realm, {realm, <<"Staff">>}}},
                         V({private, [Staff, {realm, <<"Staff">>}]})),
           % an empty realm is allowed and authorises nobody
           ?_assertMatch({ok, {private, [None], []}},
                         V({private, None})),
           ?_assertMatch({ok, {private, [Staff, None], [_]}},
                         V({private, [Staff, None]})),
           ?_assertEqual({error_code, cert_not_authorised},
                         Auth({private, None}, Peer)),
           ?_assertEqual({error, {invalid_realm_name, <<>>}},
                         V({private, {realm, <<>>}})),
           ?_assertEqual({error, {invalid_realm_name, nope}},
                         V({private, [{realm, nope}]})),
           ?_assertEqual({error, invalid_authz_policy},
                         V({private, [Staff, "file.pem"]})),
           % without a table a realm is unknown
           ?_assertEqual({error, {unknown_realm, Staff}},
                         valid_authz_policy({private, Staff})),
           % the old file form still works beside realms
           ?_assertMatch({ok, {private, [CA], [_]}}, V({private, [CA]})),
           ?_assertMatch({ok, {private, [], []}}, V({private, []})),
           % any one of the named realms authorises
           ?_assertEqual(authorised, Auth({private, Staff}, Peer)),
           ?_assertEqual({error_code, cert_not_authorised},
                         Auth({private, Staff}, PeerCA2)),
           ?_assertEqual(authorised, Auth({private, [Staff, Ops]}, Peer)),
           ?_assertEqual(authorised,
                         Auth({private, [Staff, Ops]}, PeerCA2)),
           ?_assertEqual({error_code, cert_not_authorised},
                         Auth({private, [Staff, Ops]}, Forged))]
      end).
