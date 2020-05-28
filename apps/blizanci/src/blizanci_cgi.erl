%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_cgi).
-include("blizanci_types.hrl").

-export([serve/3]).

-spec serve(binary(), map(), server_config()) -> gemini_response().
serve(Path, Req, #server_config{
                    hostname=Hostname,
                    port=Port,
                    docroot=Docroot}) ->
    PathElements = [Docroot, "..", "cgi-bin", binary_to_list(Path)],
    {ok, Cmd} = fix_path(filename:join(PathElements)),
    Args = [Cmd],
    Env = cgi_environment(Path, Cmd, Hostname, Req, Port),

    {ok, Pid, OsPid} = exec:run(Args, [monitor,
                                      {env, Env},
                                      stdout,
                                      stderr]),
    {init_cgi, Pid, OsPid}.

fix_path(S) ->
    {ok, S2} = realpath:normalise(S),
    {ok, S3} = realpath:canonicalise(S2),
    {ok, S3}.

cgi_environment(Path, Bin, Hostname, Req, Port) ->
    Env0 = make_environment(Path, Bin, Hostname, Req, Port),
    sanitise(Env0).

make_environment(Path, Bin, Hostname, Req, Port) ->
    ScriptName = "/cgi-bin/" ++ binary_to_list(Path),
    #{ query := QueryString,
       client_cert := Cert } = Req,
    CommonName = case blizanci_x509:peercert_cn(Cert) of
        {common_name, CN} -> CN;
        _ -> ""
    end,
    [
     {"PATH_TRANSLATED", Bin},
     {"QUERY_STRING", QueryString},
     {"REMOTE_USER", CommonName},
     {"SCRIPT_NAME", ScriptName},
     {"SERVER_NAME", Hostname},
     {"SERVER_PORT", Port},
     {"SERVER_PROTOCOL", "GEMINI"}
    ].

sanitise(Env) ->
    [ sanitise_kv(K, V) || {K, V} <- Env ].

sanitise_kv(Key, Value) when is_binary(Value) ->
    {Key, binary_to_list(Value)};

sanitise_kv(Key, Value) when is_integer(Value) ->
    {Key, integer_to_list(Value)};

sanitise_kv(Key, Value) ->
    {Key, Value}.
