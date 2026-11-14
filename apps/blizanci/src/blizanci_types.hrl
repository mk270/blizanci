%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

-type cgi_status()   :: {pid(), integer(), binary()}.
-type servlet_proc() :: {'proc', pid()} | 'no_proc'.

-type mp() :: {re_pattern, _, _, _, _}.

-include_lib("public_key/include/public_key.hrl").

% An authentication realm: a named set of CA certificates.  Built only by
% blizanci_auth:realm/1, which checks the name.
-type realm() :: {'realm', binary()}.

% The global realm table: for each realm, its CA files and every
% certificate decoded from them (see blizanci_auth:add_realm/3).
-type realm_table() :: #{realm() => {[string()], [#'OTPCertificate'{}]}}.

% What a route is configured with: for private, the CA files (old form)
% or one realm or a list of realms, any of which authorises.
-type authorisation_config() :: public | restricted
                              | {private, [string()]}
                              | {private, realm() | [realm()]}.

% What a built route holds (see blizanci_auth:valid_authz_policy/2): for
% private, the CA files (or realms) and every certificate decoded from
% them, once, when the route was built.
-type authorisation() :: public | restricted
                       | {private, [string()] | [realm()],
                          [#'OTPCertificate'{}]}.

-type route_option() :: {atom(), any()}.

% How a route selects requests (see blizanci_router): a mount, by whole
% path segments, holding the mount path without leading or trailing
% slash (<<>> for "/"); an anchored regex; or an old-format tuple's
% regex, which is searched for, unanchored, as it always was.
-type selector() :: {'mount', binary()}
                  | {'regex', mp()}
                  | {'legacy', mp()}.

-record(route,
        {proto       :: atom(),
         selector    :: selector(),
         module      :: module(),
         auth_policy :: authorisation(),
         options     :: map()}). % [route_option()]}).
-type route() :: #route{}.

-record(site,
        {hostname :: binary(),
         certfile :: string(),
         keyfile  :: string(),
         routing  :: [route()]}).
-type site() :: #site{}.

%% The fields are not trusted to have the right types until validated (see
%% blizanci_config:validate/1), since they may come from unchecked config.
-record(global,
        {port     :: term(),
         servlets :: term()}).
-type global_config() :: #global{}.

-record(config,
        {global :: global_config(),
         sites  :: [site()]}).
-type config() :: #config{}.

-record(server_config,
        {hostname   :: binary(),
         port       :: integer(),
         routing    :: [route()]}).
-type server_config() :: #server_config{}.

%% Raw result of asking TLS for the peer certificate (DER or error).
-type raw_peer_cert() :: {'ok', public_key:der_encoded() } |
                         {'error', term() }.

-record(state,
        {transport    :: atom(),
         socket       :: any(),
         buffer       :: binary(),
         config       :: server_config(),
         requested    :: boolean(),
         servlet_proc :: servlet_proc(),
         client_cert  :: raw_peer_cert()}).
-type state() :: #state{}.

-type gemini_response() :: {'file', binary(), binary()}
                         | {'error_code', atom()}
                         | {'redirect', binary()}
                         | hangup
                         | none
                         | {'init_servlet', pid()}
                         | {'servlet_output', binary()}
                         | {'success', binary(), binary()}.

-type gemini_session() :: continue
                        | finished
                        | {'expect_servlet', pid()}.

-type env_list() :: [{string(), string() | binary()} | string()].


-type cgi_error() :: 'cgi_exec_error'
                   | 'cgi_output_too_large'
                   | 'file_not_found'
                   | 'gateway_busy'
                   | 'unimplemented'
                   | 'cert_not_parsed'
                   | 'cert_unsupported_encoding'.

-type gateway_result() :: {'gateway_output', binary()}
                        | {'gateway_finished', gemini_response() }
                        | {'gateway_error', cgi_error()}
                        | {'gateway_started', pid()}.


-type servlet_result() :: {'servlet_failed', atom()}
                        | {'servlet_complete', binary() }.

-type cert_details() :: #{ common_name := binary() | 'undefined',
                           issuer_common_name := binary() | 'undefined' }.

%% Decoded result of blizanci_x509:peercert_cn/1 (names, not DER).
-type client_cert_details() :: 'error' | {'ok', cert_details()}
                             | {'error', atom()}.

-type request_details() :: #{ client_cert   := raw_peer_cert(),
                              query         := binary(),
                              rest_of_input := binary(),
                              scheme        := binary(),
                              host          := binary(),
                              port          := integer(),
                              path          := binary()
                            }.

-type path_matches() :: #{ binary() => binary() }.
