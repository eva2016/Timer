%%% -----------------------------------------------------------
%%% @author eva
%%% @date 2021.09.14
%%% @doc 服务器监控树
%%% @end
%%% -----------------------------------------------------------
-module(server_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/2, init/1]).

start_link() ->
    {ok, SupPid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    %% start_child
    server_sup:start_child(serv_test, []),
    {ok, SupPid}.

start_child(Mod, Args) ->
    {ok, _} = supervisor:start_child(?MODULE,
        {Mod, {Mod, start_link, Args},
        transient, 100, worker, [Mod]}),
    ok.

init([]) ->
    {ok, {
        {one_for_one, 100, 10},
        []
    }}.