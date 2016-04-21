%%%----------------------------------------------------------------------
%%% File    : .erl
%%% Author  : LittleTwoLee <bruce>
%%% Purpose :  
%%% Created :  by LittleTwoLee <bruce>
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
	 unauthorized_response/1, 
	 badrequest_response/0,
	 json_response/2,
	 timestamp/0,
	 random_token/0,
	 hash_sha256_string/1
	 http_send/3]).

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
unauthorized_response(D) ->
    {401, ?HEADER(?CT_XML),
     #xmlel{name = <<"h1">>, attrs = [],
            children = [{xmlcdata, <<D>>}]}}.

badrequest_response() ->
    {400, ?HEADER(?CT_XML),
     #xmlel{name = <<"h1">>, attrs = [],
            children = [{xmlcdata, <<"400 Bad Request">>}]}}.
json_response(Code, Body) when is_integer(Code) ->
    {Code, ?HEADER(?CT_JSON), Body}.
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
%%%SHA256 String
%%%------------------------------------------------------------
http_send(Mothed, Url, ContentType, Data) ->
    case httpc:request(Mothed, Url, [], ContentType, Data}, [], []) of   
        {ok, {_,_,Result}}-> Result;  
        {error, {_,_,Result}}->io:format("error cause ~p~n",[Result])
    end.
