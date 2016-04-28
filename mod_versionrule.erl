%%%----------------------------------------------------------------------
%%% File    : mod_versionrule.erl
%%% Author  : LittleTwoLee <bruce>
%%% Purpose : Handle get rule 
%%% Created : 21 Apr 2016 by LittleTwoLee <bruce>
%%%----------------------------------------------------------------------

-module(mod_versionrule).

-author('bruce').

-behaviour(gen_mod).

-export([start/2, stop/1, getrule/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("logger.hrl").
-include("ejabberd_http.hrl").

-record(kv, {key, value, outtime = 0}).

start(_Host, _Opts) ->
    ok.

stop(_Host) ->
    ok.

getrule(Version) ->
    case mod_redis:get_v(Version) of
	err ->
	    getrulebyhttp(Version);
	undefined ->
	    getrulebyhttp(Version);
	Other -> 
	    Other
    end.
getrulebyhttp(Version) ->
    Url = binary_to_list(list_to_bitstring(["http://localhost:8080/api/versionrule/rule/",Version])),
    Result = tools:http_get(get, Url),
    {List} = jiffy:decode(Result),
    {_, Rule} = lists:keyfind(<<"rule">>, 1, List),
    case mod_redis:set_kv(#kv{key = Version, value = <<[1,2]>>}) of
	ok -> Rule;
	err -> err
    end.
