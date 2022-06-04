%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% To the extent (if any) permissible by law, Copyright (C) 2020  Martin Keegan
%%
%% This programme is free software; you may redistribute and/or modify it under
%% the terms of the Apache Software Licence v2.0.

-module(blizanci_wp).
-behaviour(blizanci_servlet).

-include("blizanci_types.hrl").

-export([serve/3, cancel/1, request/3]).

-type options() :: #{ bare_mimetype := binary(),
                      unknown_mimetype := binary(),
                      docroot := string(),
                      index := string(),
                      authorisation := 'public' | 'restricted' | 'secret'
                    }.

-define(INDEX, "index.gemini").
-define(BARE_MIMETYPE, <<"text/gemini">>).
-define(UNKNOWN_MIMETYPE, <<"application/octet-stream">>).
-define(DEFAULT_DOCROOT, <<"./public_gemini">>).

-spec cancel(pid()) -> ok.
cancel(_) ->
    ok.

% Called by the servlet
%
-spec request(path_matches(), request_details(), options()) ->
                         {'immediate', gemini_response()} |
                         'defer'.
request( #{ <<"PATH">> := <<"">> }, Req, #{ target := Target } ) ->
       	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = 
	   httpc:request( Target ++ "/wp-json/wp/v2/posts?per_page=100&_fields=title,slug,link"), 
	{immediate, {servlet_output, posts_to_gemini( Body, Req ) }};
request( #{ <<"PATH">> := Slug }, Req, #{ target := Target } ) ->
       	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = 
	   httpc:request( Target ++ "/wp-json/wp/v2/posts?_fields=title,link,content&slug=" ++ Slug ), 
	{immediate, {servlet_output, posts_to_gemini( Body, Req ) }}.

-spec serve(path_matches(), request_details(), options()) ->
                   gateway_result().
serve(_, _, _) ->
    {gateway_error, unimplemented}.


% Translating JSON response to GEMINI
posts_to_gemini( Body, #{ host := Host, path := Path, scheme := Scheme } ) -> 
	Base = io_lib:format("~s://~s~s", [ Scheme, Host, Path ] ),
	lists:filtermap( fun(X) -> post_to_gemini(X, Base) end, jsx:decode( binary:list_to_bin(Body) ) ).

% Search result
post_to_gemini( #{ <<"slug">> := Slug, <<"title">> := #{ <<"rendered">> := Title } }, Base ) ->
	{ true, io_lib:format( "=>~s~s ~s~n", [ Base, Slug, Title ] ) };

% Article
post_to_gemini( #{ <<"title">> := #{ <<"rendered">> := Title }, <<"content">> := #{ <<"rendered">> := Content } }, _ ) ->
	{ true, io_lib:format( "# ~s~n~s~n", [ Title, html2gemini( Content ) ] ) };

% Catch-all: Ignore
post_to_gemini( X, _ ) -> 
	io:format("-> ~p~n", [ X ] ),
	false.

% translating HTML to Gemini
%
% Data to render HTML element in Gemini: Links will be put in the footnotes to be rendered later.
-record( element, { type, text = <<"">>, opts, footnote = <<"">>, nrfoot = 0 } ).

% first parse the html (Let's use the library, we don't want to handle character encoding and stuff)
html2gemini( Content ) ->
	F = fun(E, _F, S) -> html_scan( E, S ) end,
        Opts = [{event_fun, F}, { user_state, [#element{}]}],
        { ok, Res, _ } = htmerl:sax( Content, Opts ),
	Res.

html_scan( { characters, Text }, [R = #element{ text = Content }|S ]) -> 
	[R#element{ text = << Content/binary, Text/binary >>}|S];
html_scan( { startElement, _, Type, _, Opts }, [T = #element{ nrfoot = NoFoot }|S] ) -> 
	[ #element{ type = Type, opts = Opts, nrfoot = NoFoot }, T |S];
html_scan( { endElement, _,  _, _ }, [Rc, Rp |S] ) -> [html_merge_element(Rc, Rp)|S];
html_scan( endDocument, [ #element{ text = T } ] ) -> T;
html_scan( E, S ) -> 
	io:format( "~p~n", [ E ] ),
	S.


% handle Element types: add the rendered text from element E2 into the enclosing element E1
html_merge_element( E2 = #element{ type = <<"div">> }, E1 ) ->
	do_html_merge_element( E2, E1 );

html_merge_element( E2 = #element{ type = <<"p">>, text = T2 }, E1 ) ->
	html_merge_flush( E2#element{ text = << $\n, T2/binary >> }, E1 );

html_merge_element( E2 =#element{ type = <<"h1">>, text = T2 }, E1 ) ->
	do_html_merge_element( E2#element{ text = << <<"\n# ">>/binary, T2/binary>> }, E1 );

html_merge_element( E2 =#element{ type = <<"h2">>, text = T2 }, E1 ) ->
	do_html_merge_element( E2#element{ text = << <<"\n## ">>/binary, T2/binary>> }, E1 );

html_merge_element( E2 =#element{ type = <<"h3">>, text = T2 }, E1 ) ->
	do_html_merge_element( E2#element{ text = << <<"\n### ">>/binary, T2/binary>> }, E1 );

html_merge_element( E2 =#element{ type = <<"li">>, text = T2 }, E1 ) ->
	do_html_merge_element( E2#element{ text = << <<"\n* ">>/binary, T2/binary>> }, E1 );

html_merge_element( E2 =#element{ type = <<"a">>, text = T2, opts = Opts, nrfoot = No }, E1 ) ->
	Href = html_merge_getOpt( <<"href">>, Opts ), 
	NoTxt = list_to_binary( io_lib:format( "~B", [ No + 1 ] ) ),
	SuperNum = supernum( No + 1 ),
	do_html_merge_element( E2#element{ footnote = << "\n=> ", 
							 Href/binary,
							 " ",
							 NoTxt/binary,
						         " ",
						         T2/binary >>,
					   text = << T2/binary, SuperNum/binary >>,
					   nrfoot = No + 1
					 }, E1#element{} );

html_merge_element( E2 =#element{ type = <<"body">> }, E1 ) ->
	html_merge_flush( E2, E1 );

html_merge_element( E2 =#element{ type = <<"html">> }, E1 ) ->
	html_merge_flush( E2, E1 );

html_merge_element( X2, X1 ) ->
	io:format( "~p~n", [ X2 ] ),
	do_html_merge_element( X2, X1 ).

html_merge_flush( E2 = #element{ text = T2, footnote = FN }, E1 ) ->
	do_html_merge_element( E2#element{ text = << $\n, T2/binary, FN/binary >>, footnote = <<"">> }, 
			       E1 ).

do_html_merge_element( #element{ text = T2, footnote = FN2, nrfoot = NoFoot },
		    Rv = #element{ text = T1, footnote = FN1 } ) ->
	io:format("footnote... ~B~n", [NoFoot] ),
	Rv#element{ text = <<T1/binary, T2/binary>>, footnote = <<FN1/binary,FN2/binary>>, nrfoot = NoFoot }.

html_merge_getOpt( Key, Opts ) ->
	case lists:keyfind( Key, 3, Opts ) of
		false 			-> <<"">>;
		{ _, _, _, Text }	-> Text
	end.

supernum( N ) ->
	Digit = string:slice( <<"⁰¹²³⁴⁵⁶⁷⁸⁹"/utf8>>, ( N rem 10 ), 1 ),
	case N div 10 of
		0 -> Digit;
		N2 -> Before = supernum(N2),
		      << Before/binary, Digit/binary >>
	end.
