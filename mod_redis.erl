%%%-------------------------------------------------------------------
%%% File    : mod_redis.erl
%%% Author  : LittleTwoLee <bruce>
%%% Purpose : Handle redis 
%%% Created : 22 Apr 2016 by LittleTwoLee <bruce>
%%%-------------------------------------------------------------------
-module(mod_redis).

-author('bruce').

-behaviour(ejabberd_config).
-behaviour(gen_mod).

-export([start/2, stop/1, set_kv/1, del_kv/1, get_v/1,
	 get_sessions/0, get_sessions/1, get_sessions/2,
	 get_sessions/3, opt_type/1]).

-record(kv, {key, value, outtime}).  

-include("ejabberd.hrl").
-include("ejabberd_sm.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-define(PROCNAME, 'ejabberd_redis_client').

%%%===================================================================
%%% API
%%%===================================================================
start(_Host, _Opts) ->
    init(),
    ok.

stop(_Host) ->
    ok.

-spec init() -> ok | {error, any()}.
init() ->
    Server = ejabberd_config:get_option(redis_server, fun iolist_to_list/1, "localhost"),
    Port = ejabberd_config:get_option(redis_port,
				      fun(P) when is_integer(P), P>0, P<65536 ->
					      P
				      end, 6379),
    DB = ejabberd_config:get_option(redis_db,
				    fun(I) when is_integer(I), I >= 0 ->
					    I
				    end, 0),
    Pass = ejabberd_config:get_option(redis_password, fun iolist_to_list/1, ""),
    ReconnTimeout = timer:seconds(ejabberd_config:get_option(redis_reconnect_timeout,
							     fun(I) when is_integer(I), I>0 -> 
								     I 
							     end, 1)),
    ConnTimeout = timer:seconds(ejabberd_config:get_option(redis_connect_timeout,
							   fun(I) when is_integer(I), I>0 -> 
								   I 
							   end, 1)),
    case eredis:start_link(Server, Port, DB, Pass, ReconnTimeout, ConnTimeout) of
	{ok, Client} ->
	    register(?PROCNAME, Client),
	    clean_table(),
	    ok;
	{error, _} = Err ->
	    ?ERROR_MSG("failed to start redis client: ~p", [Err]),
	    Err
    end.

-spec set_kv(#kv{}) -> ok.
set_kv(KV) ->
    term_to_binary(KV),
    K = KV#kv.key,
    V = KV#kv.value,
    T = KV#kv.outtime,
    case T of
	T when T =< 0 ->
	    case eredis:q(?PROCNAME, ["SET", K, V]) of
		{ok, <<"OK">>} -> ok;
		{err, <<"err">>} -> err
	    end;
	T when T > 0 ->
	    case eredis:q(?PROCNAME, ["SETEX", K, T, V]) of
		{ok, <<"OK">>} -> ok;
		{err, <<"err">>} -> err
	    end
    end.
-spec del_kv(binary()) -> ok | err.
del_kv(K) ->
   case eredis:q(?PROCNAME, ["DEL", k]) of
       {ok, <<"OK">>} -> ok;
       {err, <<"err">>} -> err
   end.

-spec get_v(binary()) -> binary() | err.
get_v(K) ->
    case eredis:q(?PROCNAME, ["GET", K]) of
	{ok, <<V>>} -> V;
	{err, <<"err">>} -> err
    end.
-spec get_sessions() -> [#session{}].
get_sessions() ->
    lists:flatmap(
      fun(LServer) ->
	      get_sessions(LServer)
      end, ejabberd_sm:get_vh_by_backend(?MODULE)).

-spec get_sessions(binary()) -> [#session{}].
get_sessions(LServer) ->
    ServKey = server_to_key(LServer),
    case eredis:q(?PROCNAME, ["HGETALL", ServKey]) of
	{ok, Vals} ->
	    decode_session_list(Vals);
	Err ->
	    ?ERROR_MSG("failed to get sessions from redis: ~p", [Err]),
	    []
    end.

-spec get_sessions(binary(), binary()) -> [#session{}].
get_sessions(LUser, LServer) ->
    USKey = us_to_key({LUser, LServer}),
    case eredis:q(?PROCNAME, ["HGETALL", USKey]) of
	{ok, Vals} when is_list(Vals) ->
	    decode_session_list(Vals);
	Err ->
	    ?ERROR_MSG("failed to get sessions from redis: ~p", [Err]),
	    []
    end.

-spec get_sessions(binary(), binary(), binary()) ->
    [#session{}].
get_sessions(LUser, LServer, LResource) ->
    USKey = us_to_key({LUser, LServer}),
    case eredis:q(?PROCNAME, ["HGETALL", USKey]) of
	{ok, Vals} when is_list(Vals) ->
	    [S || S <- decode_session_list(Vals),
		  element(3, S#session.usr) == LResource];
	Err ->
	    ?ERROR_MSG("failed to get sessions from redis: ~p", [Err]),
	    []
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
iolist_to_list(IOList) ->
    binary_to_list(iolist_to_binary(IOList)).

us_to_key({LUser, LServer}) ->
    <<"ejabberd:sm:", LUser/binary, "@", LServer/binary>>.

server_to_key(LServer) ->
    <<"ejabberd:sm:", LServer/binary>>.

us_sid_to_key(US, SID) ->
    term_to_binary({US, SID}).

sid_to_key(SID) ->
    term_to_binary(SID).

decode_session_list([_, Val|T]) ->
    [binary_to_term(Val)|decode_session_list(T)];
decode_session_list([]) ->
    [].

clean_table() ->
    ?INFO_MSG("Cleaning Redis SM table...", []),
    lists:foreach(
      fun(LServer) ->
	      ServKey = server_to_key(LServer),
	      case eredis:q(?PROCNAME, ["HKEYS", ServKey]) of
		  {ok, []} ->
		      ok;
		  {ok, Vals} ->
		      Vals1 = lists:filter(
				fun(USSIDKey) ->
					{_, SID} = binary_to_term(USSIDKey),
					node(element(2, SID)) == node()
				end, Vals),
		      Q1 = ["HDEL", ServKey | Vals1],
		      Q2 = lists:map(
			     fun(USSIDKey) ->
				     {US, SID} = binary_to_term(USSIDKey),
				     USKey = us_to_key(US),
				     SIDKey = sid_to_key(SID),
				     ["HDEL", USKey, SIDKey]
			     end, Vals1),
		      Res = eredis:qp(?PROCNAME, [Q1|Q2]),
		      case lists:filter(
			     fun({ok, _}) -> false;
				(_) -> true
			     end, Res) of
			  [] ->
			      ok;
			  Errs ->
			      ?ERROR_MSG("failed to clean redis table for "
					 "server ~s: ~p", [LServer, Errs])
		      end;
		  Err ->
		      ?ERROR_MSG("failed to clean redis table for "
				 "server ~s: ~p", [LServer, Err])
	      end
      end, ejabberd_sm:get_vh_by_backend(?MODULE)).

opt_type(redis_connect_timeout) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(redis_db) ->
    fun (I) when is_integer(I), I >= 0 -> I end;
opt_type(redis_password) -> fun iolist_to_list/1;
opt_type(redis_port) ->
    fun (P) when is_integer(P), P > 0, P < 65536 -> P end;
opt_type(redis_reconnect_timeout) ->
    fun (I) when is_integer(I), I > 0 -> I end;
opt_type(redis_server) -> fun iolist_to_list/1;
opt_type(_) ->
    [redis_connect_timeout, redis_db, redis_password,
     redis_port, redis_reconnect_timeout, redis_server].
