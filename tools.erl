%%%----------------------------------------------------------------------
%%% File    : tools.erl
%%% Author  : LittleTwoLee <bruce>
%%% Purpose : tools class
%%% Created : 21 Apr 2016 by LittleTwoLee <bruce>
%%%----------------------------------------------------------------------

-module(tools).

-author('bruce').

-behaviour(gen_mod).
%-include("ejabberd.hrl").
-include("jlib.hrl").
%-include("logger.hrl").
%-include("ejabberd_http.hrl").

-define(CT_PLAIN,
        {<<"Content-Type">>, <<"text/plain">>}).

-define(CT_XML,
        {<<"Content-Type">>, <<"text/xml; charset=utf-8">>}).

-define(CT_JSON,
        {<<"Content-Type">>, <<"application/json">>}).

-define(AC_ALLOW_ORIGIN,
        {<<"Access-Control-Allow-Origin">>, <<"*">>}).

-define(AC_ALLOW_METHODS,
        {<<"Access-Control-Allow-Methods">>,
         <<"GET, POST, OPTIONS">>}).

-define(AC_ALLOW_HEADERS,
        {<<"Access-Control-Allow-Headers">>,
         <<"Content-Type">>}).

-define(AC_MAX_AGE,
        {<<"Access-Control-Max-Age">>, <<"86400">>}).

-define(OPTIONS_HEADER,
        [?CT_PLAIN, ?AC_ALLOW_ORIGIN, ?AC_ALLOW_METHODS,
         ?AC_ALLOW_HEADERS, ?AC_MAX_AGE]).

-define(HEADER(CType),
        [CType, ?AC_ALLOW_ORIGIN, ?AC_ALLOW_HEADERS]).


-export([start/2,
	 stop/1,
	 unauthorized_response/0, 
	 badrequest_response/0,
	 file_response/3,
	 json_response/2,
	 json_response/1,
	 timestamp/0,
	 random_token/0,
	 hash_sha256_string/1,
	 http_get_file/2,
	 http_get/2,
	 http_post/4,
	 getconfig/3]).

start(_Host, _Opts) ->
    ok.
stop(_Host) ->
    ok.

format_result(Atom, {Name, atom}) ->
    {jlib:atom_to_binary(Name), jlib:atom_to_binary(Atom)};

format_result(Int, {Name, integer}) ->
    {jlib:atom_to_binary(Name), Int};

format_result(String, {Name, string}) ->
    {jlib:atom_to_binary(Name), iolist_to_binary(String)};

format_result(Code, {Name, rescode}) ->
    {jlib:atom_to_binary(Name), Code == true orelse Code == ok};

format_result({Code, Text}, {Name, restuple}) ->
    {jlib:atom_to_binary(Name),
     {[{<<"res">>, Code == true orelse Code == ok},
       {<<"text">>, iolist_to_binary(Text)}]}};

format_result(Els, {Name, {list, {_, {tuple, [{_, atom}, _]}} = Fmt}}) ->
    {jlib:atom_to_binary(Name), {[format_result(El, Fmt) || El <- Els]}};

format_result(Els, {Name, {list, Def}}) ->
    {jlib:atom_to_binary(Name), [element(2, format_result(El, Def)) || El <- Els]};

format_result(Tuple, {_Name, {tuple, [{_, atom}, ValFmt]}}) ->
    {Name2, Val} = Tuple,
    {_, Val2} = format_result(Val, ValFmt),
    {jlib:atom_to_binary(Name2), Val2};

format_result(Tuple, {Name, {tuple, Def}}) ->
    Els = lists:zip(tuple_to_list(Tuple), Def),
    {jlib:atom_to_binary(Name), {[format_result(El, ElDef) || {El, ElDef} <- Els]}};

format_result(404, {_Name, _}) ->
    "not_found".
unauthorized_response() ->
    {401, ?HEADER(?CT_XML),
     #xmlel{name = <<"h1">>, attrs = [],
            children = [{xmlcdata, <<"Unauthorized">>}]}}.

badrequest_response() ->
    {400, ?HEADER(?CT_XML),
     #xmlel{name = <<"h1">>, attrs = [],
            children = [{xmlcdata, <<"400 Bad Request">>}]}}.
file_response(Code, Body, ContentType) when is_integer(Code) ->
    {Code, ?HEADER({<<"Content-Type">>, ContentType}), Body}.
json_response(Code, Body) when is_integer(Code) ->
    {Code, ?HEADER(?CT_JSON), Body}.
json_response(Code) when is_integer(Code) ->
    case Code of
	501 -> {Code, ?HEADER(?CT_JSON), <<"501 Not implemented">>};
	403 -> {Code, ?HEADER(?CT_JSON), <<"403 Forbidden">>};
	401 -> {Code, ?HEADER(?CT_JSON), <<"401 Unauthorized">>};
	414 -> {Code, ?HEADER(?CT_JSON), <<"414 Request URI too long">>};
	405 -> {Code, ?HEADER(?CT_JSON), <<"405 Method not allowed">>};
	501 -> {Code, ?HEADER(?CT_JSON), <<"501 Not implemented">>};
	503 -> {Code, ?HEADER(?CT_JSON), <<"503 Service unavailable">>};
        500 -> {Code, ?HEADER(?CT_JSON), <<"500 Internal server error">>};
        400 -> {Code, ?HEADER(?CT_JSON), <<"400 Bad Request">>};
        404 -> {Code, ?HEADER(?CT_JSON), <<"404 Not found">>};
	413 -> {Code, ?HEADER(?CT_JSON), <<"413 Request entity too large">>};
	406 -> {Code, ?HEADER(?CT_JSON), <<"406 ">>}
    end.

%%%------------------------------------------------------------
%%%添加时间戳
%%%------------------------------------------------------------
timestamp() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.

%%%------------------------------------------------------------
%%%GUID
%%%------------------------------------------------------------
random_token() ->
    Term = term_to_binary({node(), make_ref()}),
    Digest = erlang:md5(Term),
    binary_to_hex(Digest).

binary_to_hex(Bin) when is_binary(Bin) ->
    [oct_to_hex(N) || <<N:4>> <= Bin].

oct_to_hex(0) -> $0;
oct_to_hex(1) -> $1;
oct_to_hex(2) -> $2;
oct_to_hex(3) -> $3;
oct_to_hex(4) -> $4;
oct_to_hex(5) -> $5;
oct_to_hex(6) -> $6;
oct_to_hex(7) -> $7;
oct_to_hex(8) -> $8;
oct_to_hex(9) -> $9;
oct_to_hex(10) -> $a;
oct_to_hex(11) -> $b;
oct_to_hex(12) -> $c;
oct_to_hex(13) -> $d;
oct_to_hex(14) -> $e;
oct_to_hex(15) -> $f.

%%%------------------------------------------------------------
%%%SHA256 String
%%%------------------------------------------------------------
hash_sha256_string(String) ->
    SHAString = crypto:hash(sha256, String),
    binary_to_list(iolist_to_binary([io_lib:format("~2.16.0b", [S]) || S <- binary_to_list(SHAString)])).

%%%------------------------------------------------------------
%%%http_get
%%%------------------------------------------------------------
http_get(Mothed, Url) ->
    case httpc:request(Mothed, {Url, []}, [], []) of 
        {ok, {_,_,Result}}-> Result;  
        {error, {_,_,Result}}->io:format("error cause ~p~n",[Result])
    end.

%%%------------------------------------------------------------
%%%http_get_file
%%%------------------------------------------------------------
http_get_file(Mothed, Url) ->
    case httpc:request(Mothed, {Url, []}, [], []) of 
        {ok, {_, Headers, Result}}-> {Headers, Result};  
        {error, {_, _, Result}}->tools:json_response(200, [Result])
    end.

%%%------------------------------------------------------------
%%%http_post
%%%------------------------------------------------------------
http_post(Mothed, Url, ContentType, Data) ->
    case httpc:request(Mothed, {Url, [], ContentType, Data}, [], []) of   
%        {ok, {_,_,Result}}-> Result;
	{ok, Result}-> Result;  
        {error, {_,_,Result}}->io:format("error cause ~p~n",[Result])
    end.

getconfig(Config_Str, Fun ,Defaults) ->
    if Fun /= null ->
	    ejabberd_config:get_option(Config_Str, Fun, Defaults);
       Fun == null ->
	    ejabberd_config:get_option(Config_Str, fun iolist_to_list/1, Defaults)
    end.
    
iolist_to_list(IOList) ->
    binary_to_list(iolist_to_binary(IOList)).
