%%%----------------------------------------------------------------------
%%% File    : mod_versionrule.erl
%%% Author  : LittleTwoLee <bruce>
%%% Purpose : Handle get rule 
%%% Created : 21 Apr 2016 by LittleTwoLee <bruce>
%%%----------------------------------------------------------------------

-module(mod_versionrule).

-author('bruce').

-behaviour(gen_mod).

-export([start/2, stop/1, checktoken/4]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").
-include("ejabberd_http.hrl").

-record(kv, {key, value, outtime = 0}).

start(_Host, _Opts) ->
    ok.

stop(_Host) ->
    ok.

-spec checktoken(binary(), binary(), binary(), binary()) -> boolean |err.
checktoken(ObjectId, Version, ParameterToken, Token) ->
    case getrule(Version) of
	err -> err;
	Rule -> 
	    ListPar = rulesplit(ParameterToken, Rule),
	    ListToken = evenlysplit(Token, Rule),
	    Result = tools:hash_sha256_string(
		       string:join(
			 lists:append(
			   lists:zipwith(fun(X, Y) -> [X, Y] end, ListPar, ListToken)), "")),
	    Pwd = fun(ObjectId) ->
			  case mod:get_v(ObjectId) of
			      undefined -> getpwdbyhttp(ObjectId);	    
			      err -> getpwdbyhttp(ObjectId);
			      Other -> Other
			  end,
	    case Pwd of
		Result -> true;
		Other -> false
	    end
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
	    Other
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
