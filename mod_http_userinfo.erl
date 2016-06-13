%%%----------------------------------------------------------------------
%%% File    : mod_http_userinfo.erl
%%% Author  : LittleTwoLee <bruce>
%%% Purpose : Handle incoming http 
%%% Created : 21 Apr 2016 by LittleTwoLee <bruce>
%%%----------------------------------------------------------------------

-module(mod_http_userinfo).

-author('bruce').

-behaviour(gen_mod).

-export([start/2, stop/1, process/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").
-include("ejabberd_http.hrl").

-record(kv, {key, value, outtime = 60}).  	

start(_Host, _Opts) ->
    ok.

stop(_Host) ->
    ok.
%% case mod_versionrule:check_permissions(Headers) of
%% 	true ->
	    
%% 	false ->
%% 	    tools:json_response(401)
%%     end;
process(_, #request{method = 'POST', 
		    path = [ <<"api">>, <<"userinfo">>, <<"create">> ], 
		    headers = Headers,
		    data = Data}) ->
    Url = tools:get_url("golangapi", "userinfo", "/create"),
    case tools:http_post(post, Url, "application/json", Data) of
    	{{_, 200, "OK"}, _, Result} ->
    	    {ResultList} = jiffy:decode(Result),
	    tools:json_response(200, Result);
    	_ ->
    	    tools:json_response(200, [jiffy:encode({[{state, <<"err">>}]})])
    end;
%%%------------------------------------------------------------
%%% api/userinfo/id?
%%%------------------------------------------------------------
process(_, #request{method = 'GET', 
		    path = [ <<"api">>, <<"userinfo">>, <<"id">> ], 
		    headers = Headers,
		    q = [{_, ObjectId}]}) ->
    Url = tools:get_url("golangapi", "userinfo", lists:append(["/id/", binary_to_list(ObjectId)])),
    Result = tools:http_get(get, Url),
    tools:json_response(200, [Result]);
%%%------------------------------------------------------------
%%% api/userinfo/delete?
%%%------------------------------------------------------------
process(_, #request{method = 'DELETE', 
		    path = [ <<"api">>, <<"userinfo">>, <<"delete">> ], 
		    headers = Headers,
		    q = [{_ , ObjectId}]}) ->
    Url = tools:get_url("golangapi", "userinfo", lists:append(["/delete/", binary_to_list(ObjectId)])),
    Result = tools:http_get(delete, Url),
    tools:json_response(200, [Result]);

process(_, #request{method = 'POST',
		    path = [ <<"api">>, <<"userinfo">>, <<"uploadpic">> ], 
		    headers = Headers,
		    data = Data}) ->
    {_, ContentType} = lists:keyfind('Content-Type', 1, Headers),
    Url = tools:get_url("golangapi", "userinfo", "/uploadpic"),
    case tools:http_post(post, Url, binary_to_list(ContentType), Data) of
    	{{_, 200, "OK"}, _, Result} ->
    	    {ResultList} = jiffy:decode(Result),
    	    tools:json_response(200, Result);
    	_ ->
    	    tools:json_response(200, [jiffy:encode({[{state, <<"err">>}]})])
    end;
process(_, #request{method = 'GET',
		    path = [ <<"api">>, <<"userinfo">>, <<"downloadpic">> ], 
		    q = [{_, ObjectId}]}) ->
    Url = tools:get_url("golangapi", "userinfo", lists:append(["/downloadpic/", binary_to_list(ObjectId)])),
    {Headers, Result} = tools:http_get_file(get, Url),
    {_, ContentType} = lists:keyfind("content-type", 1, Headers),
    tools:file_response(200, Result, ContentType);
process(_, _) ->
    tools:json_response(404, "").

%%%===================================================================
%%% Internal functions
%%%===================================================================
