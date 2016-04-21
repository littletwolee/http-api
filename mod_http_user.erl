%%%----------------------------------------------------------------------
%%% File    : mod_http_user.erl
%%% Author  : LittleTwoLee <bruce>
%%% Purpose : Handle incoming http 
%%% Created : 21 Apr 2016 by LittleTwoLee <bruce>
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

process(_, #request{method = 'POST', 
		    path = [ <<"api">>, <<"user">>, <<"register">> ], 
		    data = Data}) ->
    {List} = jiffy:decode(Data),
    {_, Name} = lists:keyfind(<<"name">>, 1, List),
    {_, Pwd} = lists:keyfind(<<"pwd">>, 1, List),
    Url = "http://localhost:8080/api/user/create",
    SendData = jiffy:encode({[{<<"name">>, Name},{<<"pwd">>, Pwd}]}),
    Result = tools:http_post(post, Url, "application/json", SendData),
    tools:json_response(200, Result);
process(_, #request{method = 'GET', 
		     path = [ <<"api">>, <<"user">>, <<"getuserbyid">> ], 
		     q = [{<<"objectId">>, ObjectId}]}) ->
    Url = binary_to_list(list_to_bitstring(["http://localhost:8080/api/user/id/",ObjectId])),
    Result = tools:http_get(get, Url),
    %% Case httpc:request(get,{Url, [], [], []},[],[]) of   
    %%     {ok, {_, _, Result}}-> Result;  
    %%     {error, {_, _, Result}}->io:format("error cause ~p~n",[Result])  
    %% end,
%    Time = "1460015905",
%    Salt = "83B214E255765F948B0ACD6954229D7B",
%    RangeNum = [[7, 15, 23, 31], [10, 15, 23, 31]],
%    HashSalt = re:replaced(string:substr(Salt, 7, 2)),
%    Token = tools:hash_sha256_string(list_to_bitstring([tools:hash_sha256_string(Time),Salt])),
    tools:json_response(200, Result);
process(_, #request{method = 'DELETE', 
		    path = [ <<"api">>, <<"user">>, <<"delete">> ], 
		    q = [{_ , ObjectId}]}) ->
    Url = binary_to_list(list_to_bitstring(["http://localhost:8080/api/user/delete/",ObjectId])),
    Result = tools:http_get(delete, Url),
    tools:json_response(200, [Result]).
%% process(_, _) ->
%%     tools:json_response(404, "").

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

    
