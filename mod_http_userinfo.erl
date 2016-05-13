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
    {List} = jiffy:decode(Data),
    io:format("~p", [List]),
    {_, Name} = lists:keyfind(<<"name">>, 1, List),
    {_, Signature} = lists:keyfind(<<"signature">>, 1, List),
    {_, Address} = lists:keyfind(<<"address">>, 1, List),
    {_, Gender} = lists:keyfind(<<"gender">>, 1, List),
    {_, Area} = lists:keyfind(<<"area">>, 1, List),
    %% SendData = jiffy:encode({[{<<"name">>, Name}, 
    %% 			      {<<"signature">>, Signature},
    %% 			      {<<"address">>, Address},
    %% 			      {<<"gender">>, Gender},
    %% 			      {<<"area">>, Area}]}),
    ?INFO_MSG(Area, []),
    io:format("~p", [Area]);
    %% case tools:http_post(post, get_url("create"), "application/json", SendData) of
    %% 	{{_, 200, "OK"}, _, Result} ->
    %% 	    {ResultList} = jiffy:decode(Result);
    %% 	_ ->
    %% 	    tools:json_response(200, [jiffy:encode({[{state, <<"err">>}]})])
    %% end;
process(_, #request{method = 'GET', 
		    path = [ <<"api">>, <<"userinfo">>, <<"id">> ], 
		    headers = Headers,
		    q = [{<<"imid">>, ObjectId}]}) ->
    Result = tools:http_get(get, get_url(lists:append(["id/", ObjectId]))),
    tools:json_response(200, Result);
process(_, #request{method = 'DELETE', 
		    path = [ <<"api">>, <<"userinfo">>, <<"delete">> ], 
		    headers = Headers,
		    q = [{_ , ObjectId}]}) ->
    Result = tools:http_get(delete, get_url(lists:append(["delete/", ObjectId]))),
    tools:json_response(200, [Result]);
process(_, _) ->
    tools:json_response(404, "").

%%%===================================================================
%%% Internal functions
%%%===================================================================
iolist_to_list(IOList) ->
    binary_to_list(iolist_to_binary(IOList)).

get_url(ParStr) ->
    Host = ejabberd_config:get_option(golangapihost, fun iolist_to_list/1, "localhost"),
    io:format("~p", [Host]),
    Port = ejabberd_config:get_option(golangapiport, fun iolist_to_list/1, "8080"),
    Mode = "userinfo",
    Url = lists:append(["http://", Host, ":", Port, "/api/", Mode, "/"]),
    lists:append([Url, ParStr]).
