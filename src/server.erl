%%% -----------------------------------------------------------
%%% @author eva
%%% @date 2021.09.14
%%% @doc 服务器app入口
%%% @end
%%% -----------------------------------------------------------
-module(server).
-behaviour(application).

-export([start/0, start/2, stop/1]).

start() ->
    ok = application:start(server),
    ok.

%% application start callback
start(_StartType, _StartArgs) ->
    {ok, SupPid} = server_sup:start_link(),
    {ok, SupPid}.

stop(_State) ->
    ok.