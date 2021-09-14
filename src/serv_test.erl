%%% -----------------------------------------------------------
%%% @author eva
%%% @date 2021.09.14
%%% @doc 测试进程
%%% @end
%%% -----------------------------------------------------------
-module(serv_test).
-behaviour(gen_server).

-export([cast/3, call/3]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

cast(Mod, Func, Args) ->
    gen_server:cast(?MODULE, {apply, Mod, Func, Args}).

call(Mod, Func, Args) ->
    gen_server:call(?MODULE, {apply, Mod, Func, Args}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({apply, Mod, Func, Args}, _From, State) ->
    Ret = erlang:apply(Mod, Func, Args),
    {reply, Ret, State};

handle_call(Req, _From, State) ->
    io_lib:format("handle_call unknown req: ~p", [Req]),
    {noreply, State}.

handle_cast({apply, Mod, Func, Args}, State) ->
    erlang:apply(Mod, Func, Args),
    {noreply, State};

handle_cast(Req, State) ->
    io_lib:format("handle_cast unknown req: ~p", [Req]),
    {noreply, State}.

handle_info(Req, State) ->
    io_lib:format("handle_info unknown req: ~p", [Req]),
    {noreply, State}.

terminate(_Reason, _State) ->
    io_lib:format("serv_test exit!", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
