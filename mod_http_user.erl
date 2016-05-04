%%%----------------------------------------------------------------------
%%% File    : mod_http_user.erl
%%% Author  : LittleTwoLee <bruce>
%%% Purpose : Handle incoming http 
%%% Created : 21 Apr 2016 by LittleTwoLee <bruce>
%%%----------------------------------------------------------------------

-module(mod_http_user).

-author('bruce').

-behaviour(gen_mod).

-export([start/2, stop/1, process/2, check_permissions/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").
-include("ejabberd_http.hrl").

-record(kv, {key, value, outtime = 60}).  


start(_Host, _Opts) ->
    ok.

stop(_Host) ->
    ok.

-spec check_permissions([]) -> boolean().
check_permissions(Headers) ->
    Authorization = case lists:keyfind('Authorization', 1, Headers) of
			{_, A} -> case is_binary(A) of
				      true -> binary_to_list(A);
				     false -> A
				  end;
    		        false -> false	
    		    end,
    Token = case lists:keyfind(<<"Token">>, 1, Headers) of
		{_, T} -> case is_binary(T) of
			      true -> binary_to_list(T);
			      false -> T
			  end;
		false -> false
	    end,
    EncryptionToken = case lists:keyfind(<<"Encryptiontoken">>, 1, Headers) of
			  {_, E} -> case is_binary(E) of
					true -> binary_to_list(E);
					false -> E
				    end;
			  false -> false
		      end,
    Version = case lists:keyfind(<<"Version">>, 1, Headers) of
			  {_, V} -> case is_binary(V) of
					true -> binary_to_list(V);
					false -> V
				    end;
			  false -> false
		      end,
    if 
	Authorization == false; Token == false; EncryptionToken == false ;Version == false-> false;
	true -> mod_versionrule:checktoken(Version, Authorization, Token, EncryptionToken)
    end.
process(_, #request{method = 'POST', 
		    path = [ <<"api">>, <<"user">>, <<"register">> ], 
		    headers = Headers,
		    data = Data}) ->
    case check_permissions(Headers) of
	true ->
	    {List} = jiffy:decode(Data),
	    {_, Name} = lists:keyfind(<<"name">>, 1, List),
	    {_, Pwd} = lists:keyfind(<<"pwd">>, 1, List),
	    Url = "http://localhost:8080/api/user/create",
	    SendData = jiffy:encode({[{<<"name">>, Name},{<<"pwd">>, Pwd}]}),
	    case tools:http_post(post, Url, "application/json", SendData) of
		{{_, 200, "OK"}, _, Result} ->
		    {ResultList} = jiffy:decode(Result),
		    {_, ObjectId} = lists:keyfind(<<"ObjectId">>, 1, ResultList),
		    if ObjectId /= "" ->
			    io:format("~p",[ObjectId]),
			    case mod_redis:set_kv(#kv{key = ObjectId, value = Pwd}) of
				ok -> 
				    tools:json_response(200, [jiffy:encode({[{state, <<"ok">>}]})]);
				err ->
				    TranUrl = binary_to_list(list_to_bitstring(["http://localhost:8080/api/user/delete/",ObjectId])),
				    tools:http_get(delete, TranUrl),
				    tools:json_response(200, [jiffy:encode({[{state, <<"err">>}]})])
			    end
		    end;
		_ ->
		    tools:json_response(200, [jiffy:encode({[{state, <<"err">>}]})])
	    end;
	false ->
	    tools:json_response(401)
    end;
process(_, #request{method = 'GET', 
		     path = [ <<"api">>, <<"user">>, <<"getuserbyid">> ], 
		     q = [{<<"objectId">>, ObjectId}]}) ->
    case check_permissions(Headers) of
	true ->
	    Url = binary_to_list(list_to_bitstring(["http://localhost:8080/api/user/id/",ObjectId])),
	    Result = tools:http_get(get, Url),
	    tools:json_response(200, Result);
	false ->
	    tools:json_response(401)
    end;
    
process(_, #request{method = 'GET',
		    path = [ <<"api">>, <<"user">>, <<"test">> ],
		    auth = HTTPAuth, headers = Headers} = Req) ->
    Name = "11",
    Token = "6b86b273ff34fce19d6b804eff5a3f5747ada4eaa22f1d49c01e52ddb7875b4b",
    %% Result = mod_versionrule:checktoken("5721e8a71d41c8212b7e9b66",
    %% 				       "1.0.0",
    %% 				       tools:hash_sha256_string(Name),
    %% 				       Token),
    %% Case httpc:request(get,{Url, [], [], []},[],[]) of   
    %%     {ok, {_, _, Result}}-> Result;  
    %%     {error, {_, _, Result}}->io:format("error cause ~p~n",[Result])  
    %% end,
%    Time = "1460015905",
%    Salt = "83B214E255765F948B0ACD6954229D7B",
%    RangeNum = [[7, 15, 23, 31], [10, 15, 23, 31]],
%    HashSalt = re:replaced(string:substr(Salt, 7, 2)),
%    Token = tools:hash_sha256_string(list_to_bitstring([tools:hash_sha256_string(Time),Salt])),
    
    case check_permissions("1.0.0", Headers) of
    	true ->
    	    tools:json_response(200, "123");
    	false ->
    	    tools:json_response(401)
    end;
%    tools:json_response(200, "");
    %% A = "6b86b273ff34fce19d6b804eff5a3f5747ada4eaa22f1d49c01e52ddb7875b4b",
    %% T = "0bb2206f1e8a88d0f9a8ebbfd595f6d59dbdbc75ed5a340fad929feac64183de",
    %% E = mod_versionrule:checktoken("1.0.0", A, T),
    %% tools:json_response(200, [E]);
process(_, #request{method = 'DELETE', 
		    path = [ <<"api">>, <<"user">>, <<"delete">> ], 
		    q = [{_ , ObjectId}]}) ->
    case check_permissions(Headers) of
	true ->
	    Url = binary_to_list(list_to_bitstring(["http://localhost:8080/api/user/delete/",ObjectId])),
	    Result = tools:http_get(delete, Url),
	    tools:json_response(200, [Result]);
	false ->
	    tools:json_response(401)
    end;
process(_, Req) ->
    io:format("~p", [Req]),
    tools:json_response(404, "").
