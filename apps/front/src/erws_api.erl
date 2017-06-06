-module(erws_api).

-include("erws_console.hrl").


% Behaviour cowboy_http_handler
-export([init/3, handle/2, terminate/3]).

% Behaviour cowboy_http_websocket_handler



% Called to know how to dispatch a new connection.
init({tcp, http}, Req, Opts) ->
    { Path, Req3} = cowboy_req:path_info(Req),
    ?CONSOLE_LOG("Request: ~p ~n", [ {Req, Path, Opts} ]),
    % we're not interested in serving any other content.
    {ok, Req3, Opts}
.
    
terminate(_Req, _State, _Reason) ->
    ok.

headers_text_plain() ->
        [ {<<"access-control-allow-origin">>, <<"*">>},  {<<"Content-Type">>, <<"text/plain">>} ].
        
headers_text_html() ->
        [ {<<"access-control-allow-origin">>, <<"*">>},  {<<"Content-Type">>, <<"text/html">>}  ].      

headers_json_plain() ->
        [ {<<"access-control-allow-origin">>, <<"*">>},  {<<"Content-Type">>, <<"application/json">>} ].
        
headers_png() ->
        [ {<<"access-control-allow-origin">>, <<"*">>},
          {<<"Cache-Control">>, <<"no-cache, must-revalidate">>},
          {<<"Pragma">>, <<"no-cache">>},
          {<<"Content-Type">>, <<"image/png">>} 
        ].
                
                
        
% Should never get here.
handle(Req, State) ->
      ?CONSOLE_LOG("====================================~nrequest: ~p ~n", [Req]),
      {Path, Req1} = cowboy_req:path_info(Req),
      {ok, Body, Req2 } = cowboy_req:body(Req1),      
      {UserId, ResReq} = auth_user(Req2, Body, State),
      case process(Path, UserId, Body, ResReq, State) of
	  {json, Json, ResReqLast, NewState }->
		?CONSOLE_LOG("got request result: ~p~n", [Json]),
		{ok, JsonReq} = cowboy_req:reply(200, headers_json_plain(), json_encode(Json), ResReqLast),
		{ok, JsonReq, NewState};
          {raw_answer, {Code, Binary, Headers }, ResReqLast, NewState } ->
		{ok, RawReq} = cowboy_req:reply(Code, Headers, Binary, ResReqLast),
		{ok, RawReq, NewState}
      end.      

terminate(_Req, _State) -> ok.

false_response(Req, State)->
   {raw_answer, {500, <<"{\"status\":\"false\"}">>, headers_json_plain() },  Req, State}.
 
 
-spec check_sign(tuple(), binary(), list())-> true|false. 
check_sign({undefined, undefined}, Body, State)->
  false;
check_sign({Sign, LocalKey}, Body, State)->
    CheckSign = generate_key(LocalKey, Body),
    ?CONSOLE_LOG("got salt result: calc sign ~p~n got sign ~p~n salt ~p~n body ~p~n ", 
                [CheckSign, Sign, LocalKey, Body ]),
    case list_to_binary(CheckSign)  of 
        Sign -> true;
        _ -> false
   end
.
% 

     
process([<<"time">>], undefined, _Body, Req, State)->
      ResTime = [{<<"deal_comission">>, <<"0.1">>},
		 {<<"use_f2a">>, false},
		 {<<"logged">>, false},
		 {<<"x-cache">>, true},
		 {<<"status">>, true}
		 ],
		 
      ResTime1  = get_usd_rate(ResTime),
      ResTime2 = get_time(ResTime1),
      ResTime3 = get_state(ResTime2),
      {json, {ResTime3}, Req, State};
process([<<"time">>], {api, UserId}, Body, Req, State)->
      ResTime = [
		  {<<"logged">>, true},
		  {<<"x-cache">>, true},
		  {<<"status">>, true},
		  {<<"deal_comission">>,  <<"0.05">> }
		  ],		
	% move spawn
	ResTime1  = get_usd_rate(ResTime),
	ResTime3 = get_time(ResTime1),
	ResTime4 = get_user_state(ResTime3, UserId),
	{json, {ResTime3}, Req, State};
process([<<"time">>], {session, undefined, _SessionKey}, _Body, Req, State)->
  false_response(Req, State);
process([<<"time">>], {session, SessionObj, SessionKey}, _Body, Req, NewState)->
      UserId = get_key_dict(SessionObj, <<"user_id">>, <<>>),
      UserIdBinary = list_to_binary(integer_to_list(UserId)),
      ResTime = [
		    {<<"logged">>, true},
		    {<<"x-cache">>, true},
		    {<<"status">>, true},
		    {<<"user_custom_id">>, get_key_dict(SessionObj, <<"user_custom_id">>, <<>>) },
		    {<<"use_f2a">>, get_key_dict(SessionObj, <<"use_f2a">>, false) },
		    {<<"deal_comission">>, get_key_dict(SessionObj, <<"deal_comission_show">>, <<"0.05">>) }
		    ],		
      {pickle_unicode, UserName } = get_key_dict(SessionObj, <<"username">>, {pickle_unicode, <<>>} ),
      % move spawn
      mcd:set(?LOCAL_CACHE, <<?KEY_PREFIX, "chat_", SessionKey/binary>>, pickle:term_to_pickle(UserName)),
      mcd:set(?LOCAL_CACHE, <<?KEY_PREFIX, "user_", UserIdBinary/binary>>, pickle:term_to_pickle(SessionKey)),   
      ResTime1  = get_usd_rate(ResTime),
      ResTime3 = get_time(ResTime1),
      ResTime4 = get_user_state(ResTime3, UserIdBinary),
      {json, {ResTime3}, Req, NewState};
process(_, _UserId, Body, Req, State)->
     ?CONSOLE_LOG("undefined request from ~p ~n",[Req]),
     false_response(Req, State).
    
 
load_user_session(SessionKey, State)->
  case mcd:get(?LOCAL_CACHE, SessionKey) of
    {ok, Val}->
	% add saving to localcache
	pickle:pickle_to_term(Val);
     _ ->
        undefined
  end.
      

auth_user(Req, Body, State)->
       {Sign, Req3 }  = cowboy_req:header(<<"api_sign">>, Req, undefined),
       {PublicKey, Req4 }  = cowboy_req:header(<<"public_key">>, Req3, undefined),
       {CookieSession, Req5} = cowboy_req:cookie(<<"sessionid">>, Req4, undefined), 
       case CookieSession of 
	  undefined ->
	    {NewState, LocalKey, UserId} = case catch dict:fetch(State, PublicKey) of
						     {'EXIT', _ } -> {State, undefined, undefined};
						     {Value, User_Id} -> {State, Value, User_Id}
					    end,				    
	    case check_sign({Sign, LocalKey}, Body, State) of 
		    true ->   { {api, UserId}, Req5};
		    false ->  ?CONSOLE_LOG("salt false ~n", []), 
                              {undefined, Req5}
	    end;
	  Session->
	      SessionObj =  load_user_session(CookieSession, State),
	      { {session, SessionObj, CookieSession}, Req5}
      end     
.
  
django_session_key(Session)->
    <<?KEY_PREFIX, "django.contrib.sessions.cache", Session/binary>>.
  
get_user_state(ResTime, UserId)->
     get_user_state(ResTime, UserId, <<"state">>).
     
get_user_state(ResTime, UserId, Key)->
  case mcd:get(?LOCAL_CACHE, <<?KEY_PREFIX, Key/binary, "_", UserId/binary>>) of
    {ok, Val}->
	Rate = pickle:pickle_to_term(Val),
	[ {Key, Rate}| ResTime];
     _ ->
        ResTime
  end.
  
get_state(ResTime)->
     get_state(ResTime, <<"state">>).

get_state(ResTime, Key)->
  case mcd:get(?LOCAL_CACHE, <<?KEY_PREFIX, Key/binary>>) of
    {ok, Val}->
	Rate = pickle:pickle_to_term(Val),
	[ {Key, Rate}| ResTime];
     _ ->
        ResTime
  end.
  

get_time(ResTime)->
     get_time(ResTime, <<"time">>).
     
get_time(ResTime, Key)->
  {MegSecs, Secx, _} = now(),
  Time = MegSecs*1000000 + Secx + 3600*3,  
  [ {Key, Time}| ResTime].
  
     
get_usd_rate(ResTime)->
     get_usd_rate(ResTime, <<"usd_uah_rate">>).
     
get_usd_rate(ResTime, Key)->
  case mcd:get(?LOCAL_CACHE, << ?KEY_PREFIX, "usd_uah_rate">>) of
    {ok, Val}->
	{pickle_unicode, Rate} = pickle:pickle_to_term(Val),
	[ {Key, Rate}| ResTime];
     _ ->
        ResTime
  end.
     

fetch_django_session( UserId)->
  fetch_django_session(UserId, <<"sessionid">>)
.

fetch_django_session(UserId, Key)->
    case mcd:get(?LOCAL_CACHE, << ?KEY_PREFIX, "user_", UserId/binary>>) of
	{ok, Val}->
	    {pickle_unicode, Rate} = pickle:pickle_to_term(Val),
	    Rate;
	_ ->
	    undefined
      end
.

get_key_dict(SessionObj,Key, Default)->
   case dict:find(Key, SessionObj) of
	{ok, Value} -> Value;
	error -> Default
   end
.


     
generate_key(Salt, Body)->
        hexstring( crypto:hash(sha256, <<Salt/binary, Body/binary >>)  ) 
.

json_decode(Json)->
        jiffy:decode(Json).

json_encode(Doc)->
        jiffy:encode(Doc).


-spec hexstring( binary() ) -> list().
hexstring(<<X:128/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~32.16.0b", [X]));
hexstring(<<X:160/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~40.16.0b", [X]));
hexstring(<<X:256/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~64.16.0b", [X]));
hexstring(<<X:512/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~128.16.0b", [X])).
