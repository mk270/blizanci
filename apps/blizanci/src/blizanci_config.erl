%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

%% @doc
%% Most of the configuration of blizanci is done via this module, albeit
%% there remain some key values hardwired into the source code of the other
%% modules.
%%
%% Generally, configuration data is hardwired into this module, or derived
%% from the application environment set in sys.config.
%%
%% This module is primarily called during the initialisation of the blizanci
%% app, before the ranch listener is started. Its output is then inherited
%% via the many processes started (directly or otherwise) by ranch.
%%
%% Invalid configuration items generally result in a crash of the process
%% which responds to a relevant Gemini request. However, future versions
%% of blizanci will become increasingly strict about verifying that the
%% configuration is valid at application start time.
%% @end

-module(blizanci_config).

-export([ssl_opts/0, proto_opts/0]).

-define(PORT, 1965).

-spec ssl_opts() -> [{atom(), term()}].
%% @doc
%% Generate the options required for ranch to initialise SSL
%% @end
ssl_opts() ->
    {ok, App} = application:get_application(),
    {ok, Cert} = get_pem_file_from_environment(App, certfile,
                                               "./ssl/certificate.pem"),
    {ok, Key} = get_pem_file_from_environment(App, keyfile,
                                              "./ssl/key.pem"),
    Port = application:get_env(App, port, ?PORT),
    VerifyFn = fun (ClientCert, Ev, Init) ->
                   blizanci_x509:verify_cert(ClientCert, Ev, Init) end,

    [{port, Port},
     {certfile, Cert},
     {keyfile, Key},
     {verify, verify_peer},
     {cacertfile, "/dev/null"},
     {verify_fun, {VerifyFn, []}},
     {versions, ['tlsv1.3']}
    ].

-spec proto_opts() -> [{atom(), term()}].
%% @doc
%% Generate the options required for ranch to initialise the TCP listener
%% @end
proto_opts() ->
    {ok, App} = application:get_application(),
    {ok, Default_Hostname} = inet:gethostname(),
    Hostname = application:get_env(App, hostname, Default_Hostname),
    Docroot = application:get_env(App, docroot, "./public_gemini"),
    CGIroot = application:get_env(App, cgiroot, "./cgi-bin"),
    Port = application:get_env(App, port, ?PORT),
    Default_Routing = routing_table(Hostname, Docroot, CGIroot, Port),
    Routes = application:get_env(App, routing, Default_Routing),
    {ok, Routing} = blizanci_router:prepare(Routes),

    [{hostname, Hostname},
     {docroot, Docroot},
     {cgiroot, CGIroot},
     {port, Port},
     {routing, Routing}
    ].

-type route_entry() :: {string(), atom(), atom(), map()}.
-spec routing_table(string(), string(), string(), integer()) -> [route_entry()].
routing_table(Hostname, Docroot, CGIroot, Port) ->
    CGI_Opts =
        #{ hostname => Hostname,
           port => Port,
           cgiroot => CGIroot,
           cgiprefix => "/cgi-bin/" },
    Static_Opts =
        #{ index => "index.gemini",
           docroot => Docroot,
           unknown_mimetype => <<"application/octet-stream">>,
           bare_mimetype => <<"text/gemini">>,
           authorisation => public
         },
    Restricted_Opts = Static_Opts#{ authorisation => restricted },
    [
     {"cgi-bin/(?<PATH>.*)",   blizanci_cgi,    public,     CGI_Opts},
     {"(?<PATH>restricted.*)", blizanci_static, restricted, Restricted_Opts},
     {"(?<PATH>.*)",           blizanci_static, public,     Static_Opts}
    ].


-spec get_pem_file_from_environment(atom(), atom(), string()) ->
          {ok, string()} | {fail, atom()}.
get_pem_file_from_environment(App, Key, Default_Filename) ->
    Value = application:get_env(App, Key, Default_Filename),
    validate_pem_file(Value).


-spec validate_pem_file(string()) -> {ok, string()} | {fail, atom()}.
validate_pem_file(Filename) ->
    case file:read_file(Filename) of
        {ok, PemBin} ->
            case public_key:pem_decode(PemBin) of
                [] ->
                    lager:warning("File ~p not a PEM cert.", [Filename]),
                    {fail, not_a_cert};
                _ -> {ok, Filename}
            end;
        _ ->
            lager:warning("Couldn't open ~p", [Filename]),
            {fail, couldnt_open_pem_file}
    end.
