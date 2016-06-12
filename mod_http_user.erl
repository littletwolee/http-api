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
-define(HTTP, "http://").
-define(API, "/api").
-define(MODULENAME, "/user").

-record(kv, {key, value, outtime = 60}).  


start(_Host, _Opts) ->
    ok.

stop(_Host) ->
    ok.

process(_, #request{method = 'POST', 
		    path = [ <<"api">>, <<"user">>, <<"register">> ], 
		    headers = Headers,
		    data = Data}) ->
    %% case mod_versionrule:check_permissions(Headers) of
    %% 	true ->
	    {List} = jiffy:decode(Data),
	    {_, Name} = lists:keyfind(<<"name">>, 1, List),
	    {_, Pwd} = lists:keyfind(<<"pwd">>, 1, List),
            GolangAPIHost = tools:getconfig(golangapihost, null, "localhost"),
            GolangAPIPort = tools:getconfig(golangapiport, null, "8080"),
            Url = binary_to_list(lists:append([?HTTP, GolangAPIHost, ":", GolangAPIPort, ?API, ?MODULENAME, "/create"])),
	    SendData = jiffy:encode({[{<<"name">>, Name},{<<"pwd">>, Pwd}]}),
	    case tools:http_post(post, Url, "application/json", SendData) of
		{{_, 200, "OK"}, _, Result} ->
		    {ResultList} = jiffy:decode(Result),
		    {_, ObjectId} = lists:keyfind(<<"ObjectId">>, 1, ResultList),
		    if ObjectId /= "" ->
			    case ejabberd_auth:try_register(Name, <<"im.com">>, Pwd) of
				{atomic, ok} ->
				    case mod_redis:set_kv(#kv{key = ObjectId, value = Pwd}) of
					ok -> 
					    tools:json_response(200, [jiffy:encode({[{state, <<"ok">>}]})]);
					err ->
					    TranUrl = binary_to_list(list_to_bitstring(["http://localhost:8080/api/user/delete/",ObjectId])),
					    tools:http_get(delete, TranUrl),
					    tools:json_response(200, [jiffy:encode({[{state, <<"err">>}]})])
				    end;
				    Error ->
					tools:json_response(200, [jiffy:encode({[{state, <<"err">>}]})])	     
			    end
			    
		    end;
		_ ->
		    tools:json_response(200, [jiffy:encode({[{state, <<"err">>}]})])
	    end;
    %% 	false ->
    %% 	    tools:json_response(401)
    %% end;
process(_, #request{method = 'GET', 
		    path = [ <<"api">>, <<"user">>, <<"login">> ], 
		    headers = Headers,
		    q = [{<<"username">>, UserName}, {<<"password">>, PassWord}]}) ->
    %% case mod_versionrule:check_permissions(Headers) of
    %% 	true ->
            GolangAPIHost = tools:getconfig(golangapihost, null, "localhost"),
            GolangAPIPort = tools:getconfig(golangapiport, null, "8080"),
            Url = binary_to_list(lists:append([?HTTP, GolangAPIHost, ":", GolangAPIPort, ?API, ?MODULENAME, "/name", UserName])),
    io:format("~p", [Url]),
	    Result = tools:http_get(get, Url),
            {List} = jiffy:decode(Result),
            {_, Pwd} = lists:keyfind(<<"Pwd">>, 1, List),
            case PassWord == Pwd of
	       true ->
	          tools:json_response(200, Result);
	      false ->
	          tools:json_response(401)
            end;
    %% 	false ->
    %% 	    tools:json_response(401)
    %% end;
process(_, #request{method = 'DELETE', 
		    path = [ <<"api">>, <<"user">>, <<"delete">> ], 
		    headers = Headers,
		    q = [{_ , ObjectId}]}) ->
    case mod_versionrule:check_permissions(Headers) of
	true ->
	    Url = binary_to_list(list_to_bitstring(["http://localhost:8080/api/user/delete/",ObjectId])),
	    GolangAPIHost = tools:getconfig(golangapihost, null, "localhost"),
            GolangAPIPort = tools:getconfig(golangapiport, null, "8080"),
            Url = binary_to_list(lists:append([?HTTP, GolangAPIHost, ":", GolangAPIPort, ?API, ?MODULENAME, "/delete", ObjectId])),
	    Result = tools:http_get(delete, Url),
	    tools:json_response(200, [Result]);
	false ->
	    tools:json_response(401)
    end;
%% process(_, #request{method = 'GET',
%% 		   path = [ <<"api">>, <<"user">>, <<"test">>]
%% 		   }) ->
%%     tools:json_response(200, [GolangAPIHost]);
process(_, Req) ->
    io:format("~p", Req),
    tools:json_response(404, "").
