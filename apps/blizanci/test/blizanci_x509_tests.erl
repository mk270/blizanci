%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-module(blizanci_x509_tests).

-include_lib("eunit/include/eunit.hrl").
-include("blizanci_types.hrl").

-import(blizanci_x509,
        [cert_key_match/2, certificates_from_file/1, check_cert/1,
         peercert_cn/1, test_cert/5, test_cn/1, test_pair_files/3,
         test_pem_cert/1, test_pki/0, test_tmp_dir/0, test_utc/1,
         test_write_pem/3, validate_ca_file/1, validate_pem_file/1,
         verify_cert/3, verify_signed_by/2]).

pki_fixture(Tests) ->
    {setup, fun blizanci_x509:test_pki/0, fun(_) -> ok end, Tests}.

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
           ?_assertNot(apply(blizanci_x509, verify_signed_by,
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
           ?_assertEqual({error, cert_not_yet_valid}, check_cert(NotYet)),
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

% Just inside and just outside each end of the validity period (30 s
% either way; the check reads the real clock).
check_cert_boundary_test_() ->
    pki_fixture(
      fun(#{peer_key := Key}) ->
          Name = test_cn("alice"),
          Now = calendar:datetime_to_gregorian_seconds(
                  calendar:universal_time()),
          Cert = fun(NotBefore, NotAfter) ->
                     test_cert(Name, Name, Key, Key,
                               {test_utc(Now + NotBefore),
                                test_utc(Now + NotAfter)})
                 end,
          [?_assertMatch({ok, _}, check_cert(Cert(-30, 3600))),
           ?_assertMatch({ok, _}, check_cert(Cert(-3600, 30))),
           ?_assertEqual({error, cert_not_yet_valid},
                         check_cert(Cert(30, 3600))),
           ?_assertEqual({error, cert_expired},
                         check_cert(Cert(-3600, -30)))]
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
            no_cn := NoCN, no_cn_issuer := NoCNIssuer,
            no_cn_both := NoCNBoth, expired := Expired}) ->
          [?_assertEqual({ok, #{ common_name => <<"alice">>,
                                issuer_common_name => <<"Test CA">> }},
                         peercert_cn({ok, Peer})),
           ?_assertEqual({ok, #{ common_name => <<"carol">>,
                                issuer_common_name => <<"Test CA">> }},
                         peercert_cn({ok, Printable})),
           % No Common Name: the name is 'undefined', for the subject,
           % the issuer, or both.
           ?_assertEqual({ok, #{ common_name => undefined,
                                issuer_common_name => <<"Test CA">> }},
                         peercert_cn({ok, NoCN})),
           ?_assertEqual({ok, #{ common_name => <<"alice">>,
                                issuer_common_name => undefined }},
                         peercert_cn({ok, NoCNIssuer})),
           ?_assertEqual({ok, #{ common_name => undefined,
                                issuer_common_name => undefined }},
                         peercert_cn({ok, NoCNBoth})),
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

junk_cert() ->
    {'Certificate', <<1, 2, 3>>, not_encrypted}.

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
           junk    => test_write_pem(Dir, "junk.pem", [junk_cert()]),
           mixed   => test_write_pem(Dir, "mixed.pem",
                                     [test_pem_cert(CA), junk_cert()]),
           text    => filename:join(Dir, "text.pem"),
           missing => filename:join(Dir, "missing.pem")},
         ok = file:write_file(maps:get(text, Files), <<"hello\n">>),
         {Dir, Pki, Files}
     end,
     fun({Dir, _, _}) -> file:del_dir_r(Dir) end,
     fun({_, #{ca_otp := CAOtp} = Pki, Files}) ->
         #{ca := CAFile, both := Both, key := KeyFile, empty := Empty,
           text := Text, missing := Missing, junk := Junk,
           mixed := Mixed} = Files,
         #{ca2_otp := CA2Otp} = Pki,
         [?_assertEqual({ok, CAFile}, validate_pem_file(CAFile)),
          ?_assertEqual({ok, Both}, validate_pem_file(Both)),
          % any PEM entry will do: it need not be a certificate
          ?_assertEqual({ok, KeyFile}, validate_pem_file(KeyFile)),
          ?_assertEqual({fail, not_a_cert}, validate_pem_file(Empty)),
          ?_assertEqual({fail, not_a_cert}, validate_pem_file(Text)),
          ?_assertEqual({fail, couldnt_open_pem_file},
                        validate_pem_file(Missing)),
          ?_assertEqual({ok, [CAOtp]}, certificates_from_file(CAFile)),
          % every certificate is returned, in file order
          ?_assertEqual({ok, [CAOtp, CA2Otp]},
                        certificates_from_file(Both)),
          % non-certificate entries are skipped
          ?_assertEqual({error, no_certificate_in_file},
                        certificates_from_file(KeyFile)),
          ?_assertEqual({error, no_certificate_in_file},
                        certificates_from_file(Empty)),
          ?_assertEqual({error, no_certificate_in_file},
                        certificates_from_file(Text)),
          ?_assertEqual({error, enoent}, certificates_from_file(Missing)),
          ?_assertEqual({error, certificate_not_decodable},
                        certificates_from_file(Junk)),
          % one bad entry refuses the whole file
          ?_assertEqual({error, certificate_not_decodable},
                        certificates_from_file(Mixed)),
          ?_assertEqual({error, not_a_path}, certificates_from_file("")),
          ?_assertEqual({error, not_a_path},
                        certificates_from_file(<<"/x">>)),
          ?_assertEqual(ok, validate_ca_file(Both)),
          ?_assertEqual({error, certificate_not_decodable},
                        validate_ca_file(Mixed))]
     end}.

% The key-pair check, on keys of every supported type.
-define(KINDS, [rsa, p256, p384, ed25519, ed448]).
% kinds the check cannot compare
-define(ODD_KINDS, [dsa, p224]).
-define(ENC_PKCS8,
        "-----BEGIN ENCRYPTED PRIVATE KEY-----\n"
        "MIH0MF8GCSqGSIb3DQEFDTBSMDEGCSqGSIb3DQEFDDAkBBAd8ooWkLz04GZ/awNS\n"
        "g/GNAgIIADAMBggqhkiG9w0CCQUAMB0GCWCGSAFlAwQBKgQQjVo+OqEoi6SNywqk\n"
        "gZRAkgSBkGgoLESlVKny7ZznVmQz0jEaN4gBWrAchXrYEOSBKFvRnWrznrNgXtzG\n"
        "H3vComwbLOHNdQyzXDdO3HXdpgOhh+IDGu0TxAG+We4SoIt1kp9CGq63d88Sgu9U\n"
        "GaabHjm3LEN7QqZfiqdcqgTet50Eh/ln/tAQAsM0ByNsN3pe4oyRgPuP1P+AoUay\n"
        "FxAk4VmWLw==\n"
        "-----END ENCRYPTED PRIVATE KEY-----\n").

cert_key_match_test_() ->
    {setup,
     fun() ->
         Dir = test_tmp_dir(),
         Pairs = maps:from_list(
                   [ {{K, N}, test_pair_files(Dir, atom_to_list(K) ++ N,
                                              K)}
                     || K <- ?KINDS ++ ?ODD_KINDS, N <- ["1", "2"] ]),
         {Dir, Pairs}
     end,
     fun({Dir, _}) -> file:del_dir_r(Dir) end,
     fun({Dir, Pairs}) ->
         Cert = fun(K, N) -> element(1, maps:get({K, N}, Pairs)) end,
         Key = fun(K, N) -> element(2, maps:get({K, N}, Pairs)) end,
         Match = fun(K1, K2) ->
                     cert_key_match(Cert(K1, "1"), Key(K2, "1"))
                 end,
         Write = fun(Name, Entries) ->
                     test_write_pem(Dir, Name, Entries)
                 end,
         Pem = fun(File) ->
                   {ok, B} = file:read_file(File),
                   public_key:pem_decode(B)
               end,
         Raw = fun(Name, Bin) ->
                   P = filename:join(Dir, Name),
                   ok = file:write_file(P, Bin),
                   P
               end,
         Missing = filename:join(Dir, "missing"),
         [ ?_assertEqual(ok, Match(K, K)) || K <- ?KINDS ]
         ++ [ ?_assertEqual({error, mismatch},
                            cert_key_match(Cert(K, "1"), Key(K, "2")))
              || K <- ?KINDS ]
         ++ [ ?_assertEqual({error, mismatch}, Match(A, B))
              || A <- ?KINDS, B <- ?KINDS, A =/= B ]
         ++ [
          % the same file may hold both certificate and key
          ?_assertEqual(ok, begin
              Both = Write("both.pem", Pem(Cert(p256, "1"))
                                       ++ Pem(Key(p256, "1"))),
              cert_key_match(Both, Both)
          end),
          % PKCS#8 wrapping of an RSA key
          ?_assertEqual(ok, begin
              [{_, Der, _}] = Pem(Key(rsa, "1")),
              Rsa = public_key:der_decode('RSAPrivateKey', Der),
              cert_key_match(Cert(rsa, "1"),
                             Write("rsa8.pem",
                                   [public_key:pem_entry_encode(
                                      'PrivateKeyInfo', Rsa)]))
          end),
          % PKCS#8 wrapping of an EC key
          ?_assertEqual(ok, begin
              [{_, Der, _}] = Pem(Key(p256, "1")),
              Ec = public_key:der_decode('ECPrivateKey', Der),
              cert_key_match(Cert(p256, "1"),
                             Write("ec8.pem",
                                   [public_key:pem_entry_encode(
                                      'PrivateKeyInfo', Ec)]))
          end),
          % a chain: the leaf is the first certificate
          ?_assertEqual(ok, begin
              Chain = Write("chain.pem", Pem(Cert(rsa, "1"))
                                         ++ Pem(Cert(p256, "1"))),
              cert_key_match(Chain, Key(rsa, "1"))
          end),
          ?_assertEqual({error, mismatch}, begin
              Chain = Write("chain2.pem", Pem(Cert(rsa, "1"))
                                          ++ Pem(Cert(p256, "1"))),
              cert_key_match(Chain, Key(p256, "1"))
          end),
          % encrypted keys
          ?_assertEqual({error, {keyfile, encrypted_private_key}},
                        cert_key_match(
                          Cert(rsa, "1"),
                          Write("enc.pem",
                                [{'RSAPrivateKey', <<1, 2, 3>>,
                                  {"AES-128-CBC", <<0:128>>}}]))),
          % a PKCS#8 key encrypted with a password, as openssl writes it
          ?_assertEqual({error, {keyfile, encrypted_private_key}},
                        cert_key_match(
                          Cert(rsa, "1"),
                          Raw("enc8.pem", <<?ENC_PKCS8>>))),
          % unreadable and garbage files
          ?_assertEqual({error, {certfile, {unreadable, enoent}}},
                        cert_key_match(Missing, Key(rsa, "1"))),
          ?_assertEqual({error, {keyfile, {unreadable, enoent}}},
                        cert_key_match(Cert(rsa, "1"), Missing)),
          ?_assertEqual({error, {certfile, no_certificate_in_file}},
                        cert_key_match(Raw("text1", <<"hello\n">>),
                                       Key(rsa, "1"))),
          ?_assertEqual({error, {certfile, no_certificate_in_file}},
                        cert_key_match(Key(rsa, "1"), Key(rsa, "1"))),
          ?_assertEqual({error, {keyfile, no_private_key}},
                        cert_key_match(Cert(rsa, "1"),
                                       Raw("text2", <<"hello\n">>))),
          ?_assertEqual({error, {keyfile, no_private_key}},
                        cert_key_match(Cert(rsa, "1"), Cert(rsa, "1"))),
          ?_assertEqual({error, {certfile, certificate_not_decodable}},
                        cert_key_match(Write("junkcert", [junk_cert()]),
                                       Key(rsa, "1"))),
          ?_assertEqual({error, {keyfile, private_key_not_decodable}},
                        cert_key_match(
                          Cert(rsa, "1"),
                          Write("junkkey",
                                [{'RSAPrivateKey', <<1, 2, 3>>,
                                  not_encrypted}]))),
          % a key type we cannot compare is neither a pass nor a failure
          ?_assertEqual({ok, unverified, {unsupported_key_type, "DSA"}},
                        cert_key_match(Cert(dsa, "1"), Key(dsa, "1"))),
          ?_assertEqual({ok, unverified,
                         {unsupported_key_type, "EC secp224r1"}},
                        cert_key_match(Cert(p224, "1"), Key(p224, "1"))),
          % the same unsupported curve, with a different key, likewise
          ?_assertEqual({ok, unverified,
                         {unsupported_key_type, "EC secp224r1"}},
                        cert_key_match(Cert(p224, "1"), Key(p224, "2"))),
          % different families are a mismatch, whatever they are
          ?_assertEqual({error, mismatch},
                        cert_key_match(Cert(rsa, "1"), Key(dsa, "1"))),
          ?_assertEqual({error, mismatch},
                        cert_key_match(Cert(dsa, "1"), Key(rsa, "1"))),
          ?_assertEqual({error, mismatch},
                        cert_key_match(Cert(p256, "1"), Key(dsa, "1"))),
          ?_assertEqual({error, mismatch},
                        cert_key_match(Cert(p224, "1"), Key(rsa, "1"))),
          ?_assertEqual({error, mismatch},
                        cert_key_match(Cert(rsa, "1"), Key(p224, "1"))),
          % different named curves are a mismatch, supported or not
          ?_assertEqual({error, mismatch},
                        cert_key_match(Cert(p224, "1"), Key(p256, "1"))),
          ?_assertEqual({error, mismatch},
                        cert_key_match(Cert(p256, "1"), Key(p224, "1"))),
          % a file that is not a key is still an error
          ?_assertEqual({error, {keyfile, private_key_not_decodable}},
                        cert_key_match(
                          Cert(dsa, "1"),
                          Write("junkdsa",
                                [{'DSAPrivateKey', <<1, 2, 3>>,
                                  not_encrypted}]))),
          ?_assertEqual({error, {keyfile, no_private_key}},
                        cert_key_match(Cert(dsa, "1"), Cert(dsa, "1")))
         ]
     end}.
