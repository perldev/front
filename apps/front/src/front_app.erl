%%%-------------------------------------------------------------------
%% @doc front public API
%% @end
%%%-------------------------------------------------------------------

-module(front_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================
routes() ->
    cowboy_router:compile([{'_',
			    [
			     {"/chat", erws_handler, []},
			     {"/[...]", erws_api, dict:new()},
			     {"/static/[...]", cowboy_static,
			      [{directory, <<"static">>},
			       {mimetypes,
				[{<<".png">>, [<<"image/png">>]},
				 {<<".jpg">>, [<<"image/jpeg">>]},
				 {<<".css">>, [<<"text/css">>]},
				 {<<".js">>,
				  [<<"application/javascript">>]}]}]}
			     ]}]).

start(_StartType, _StartArgs) ->
    
    inets:start(),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(compiler),
    ok = application:start(dht_ring),
    ok = application:start(syntax_tools),
    ok = application:start(emysql),
    ok = application:start(goldrush),    
    ok = application:start(lager),
    Dispatch = routes(),
    ok = case cowboy:start_http(
                listener, 100,
                [{port, 4000}],
                [{env, [{dispatch, Dispatch}]}]) of
             {ok, _} -> ok;
             {error, {already_started, _}} -> ok;
             {error, _} = Error -> Error
         end,
    front_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
