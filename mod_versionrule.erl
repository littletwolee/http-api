%%%----------------------------------------------------------------------
%%% File    : mod_versionrule.erl
%%% Author  : LittleTwoLee <bruce>
%%% Purpose : Handle get rule 
%%% Created : 21 Apr 2016 by LittleTwoLee <bruce>
%%%----------------------------------------------------------------------

-module(mod_versionrule).

-author('bruce').

-behaviour(gen_mod).

-export([start/2, stop/1, checktoken/4, check_permissions/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").
-include("ejabberd_http.hrl").

-record(kv, {key, value, outtime = 0}).

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
    
    case Authorization /= false andalso Token /= false andalso EncryptionToken /= false andalso Version /= false andalso
	string:len(Authorization) == 64 andalso string:len(Token) == 64 andalso string:len(EncryptionToken) == 64 of
	true -> mod_versionrule:checktoken(Version, Authorization, Token, EncryptionToken);
	false -> false
    end.

-spec checktoken(binary(), binary(), binary(), binary()) -> boolean() | err.
checktoken(Version, Authorization, Token, EncryptionToken) ->
    case getrule(Version) of
	err -> err;
	Rule -> 
	    ListPar = rulesplit(Authorization, Rule),
	    ListToken = rulesplit(Token, Rule),
	    string:equal(
	      EncryptionToken, tools:hash_sha256_string(
				 string:join(
				   lists:append(
				     lists:zipwith(fun(X, Y) -> [X, Y] end, ListPar, ListToken)), "")))
	    
    end.
    
-spec evenlysplit(binary(), integer) -> [] | err.
evenlysplit(Sha, EvenlyLen) ->
    evenlysplit(Sha, EvenlyLen, []).
evenlysplit([], EvenlyLen, Arr) ->
    Arr;
evenlysplit(Sha, EvenlyLen, Arr) ->
    evenlysplit(
      string:substr(Sha, EvenlyLen + 1), 
      EvenlyLen, 
      lists:append([Arr, [string:substr(Sha, 1, EvenlyLen)]])).

-spec rulesplit(binary(), []) -> ok | err.
rulesplit(Sha, Rule) ->
    rulesplit(Sha, Rule, []).
rulesplit([], Rule, Arr) ->
    Arr;
rulesplit(Sha, Rule, Arr) ->
    rulesplit(
      string:substr(Sha, hd(Rule) + 1), 
      lists:sublist(Rule, 2, length(Rule) -1), 
      lists:append([Arr, [string:substr(Sha, 1, hd(lists:sublist(Rule, 1)))]])).

-spec getrule(binary()) -> [] | err.
getrule(Version) ->
    case mod_redis:get_v(Version) of
	err ->
	    getrulebyhttp(Version);
	undefined ->
	    getrulebyhttp(Version);
	Other -> 
	    binary_to_list(Other)
    end.

-spec getrulebyhttp(binary()) -> [] | err.
getrulebyhttp(Version) ->
    Url = binary_to_list(list_to_bitstring(["http://localhost:8080/api/versionrule/rule/",Version])),
    Result = tools:http_get(get, Url),
    {List} = jiffy:decode(Result),
    {_, Rule} = lists:keyfind(<<"rule">>, 1, List),
    case mod_redis:set_kv(#kv{key = Version, value = list_to_binary(Rule)}) of
	ok -> Rule;
	err -> err
    end.

-spec getpwdbyhttp(binary()) -> binary() | err.
getpwdbyhttp(ObjectId) ->
    Url = binary_to_list(list_to_bitstring(["http://localhost:8080/api/user/id/",ObjectId])),
    {List} = jiffy:decode(tools:http_get(get, Url)),
    {_, Pwd} = lists:keyfind(<<"Pwd">>, 1, List),
    Pwd.
