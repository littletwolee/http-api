%%%----------------------------------------------------------------------
%%% File    : mod_http_user.erl
%%% Author  : LittleTwoLee <bruce>
%%% Purpose : Handle incoming http 
%%% Created : 21 Apr 2015 by LittleTwoLee <bruce>
%%%----------------------------------------------------------------------

-module(mod_http_user).

-author('bruce').

-behaviour(gen_mod).

-export([start/2, stop/1, process/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").
-include("ejabberd_http.hrl").

start(_Host, _Opts) ->
    ok.

stop(_Host) ->
    ok.

process(_, #request{method = 'POST', path = [<<"user">>,<<"register">>], data = Data}) ->
%, q = [{<<"v">>,V},{<<"k">>,K}]

%    tools:unauthorized_response(Data).
%    io:format("~p",[LPath]),
%    U = erlang:list_to_bitstring([V,"+++", K]),
%    {List} = jiffy:decode(Data),
%    {_, UserName} = lists:keyfind(<<"username">>, 1, List),
%    {_, PassWord} = lists:keyfind(<<"password">>, 1, List),
    tools:json_response(200, "22");
process([], #request{method = 'GET', q = [{<<"k">>,K}]}) ->
    io:format("~p",[K]),
%    Time = "1460015905",
%    Salt = "83B214E255765F948B0ACD6954229D7B",
%    RangeNum = [[7, 15, 23, 31], [10, 15, 23, 31]],
%    HashSalt = re:replaced(string:substr(Salt, 7, 2)),
%    Token = tools:hash_sha256_string(list_to_bitstring([tools:hash_sha256_string(Time),Salt])),
    tools:json_response(200, K).

%splicesalt(RangeNum, Num, Str, Salt) ->
%    if
%	Str == "" ->
%	    StrOld = string:substr(Salt, 0, Num -1);
%        Str /= "" ->
%            StrOld = Str
%    end,
%    if
%	string:len(Str) == 32 ->
%	    Str;
%	true ->
%	    [RanList, RepList] = RangeNum,
%	    this:splicesalt(RangeNum, Num + 1, list_to_binary([StrOld, 
%							  lists:nth(Num, RepList), 
%							  string:substr(Salt, 
%									lists:nth(Num, RanList) + 2, 
%									string:len(Salt) - lists:nth(Num, RanList) + 2)
%							 ]))
%    end.

    
