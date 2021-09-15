%%% -----------------------------------------------------------
%%% @author eva
%%% @date 2021.09.14
%%% @doc 测试进程
%%% @end
%%% -----------------------------------------------------------
-module(serv_test).
-behaviour(gen_server).

-include("time_heap.hrl").

-export([cast/1, call/1]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

cast(Req) ->
    gen_server:cast(?MODULE, Req).

call(Req) ->
    gen_server:call(?MODULE, Req).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(Req, From, State) ->
    try
        do_call(Req, From, State)
    catch
        _:Error:Stacktrace ->
            io:format("handle_call error: ~p~n", [{Req, From, Error, Stacktrace}]),
            {noreply, State}
    end.

handle_cast(Req, State) ->
    try
        do_cast(Req, State)
    catch
        _:Error:Stacktrace ->
            io:format("handle_cast error: ~p~n", [{Req, Error, Stacktrace}]),
            {noreply, State}
    end.

handle_info(Req, State) ->
    try
        do_info(Req, State)
    catch
        _:Error:Stacktrace ->
            io:format("handle_info error: ~p~n", [{Req, Error, Stacktrace}]),
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    io:format("serv_test exit!", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_call({apply, Mod, Func, Args}, _From, State) ->
    Ret = erlang:apply(Mod, Func, Args),
    {reply, Ret, State};

do_call({add_timer, HeapName, Timer}, _From, State) ->
    {Time, _} = timer:tc(time_heap, add_timer, [HeapName, Timer]),
    {reply, Time, State};

do_call({top, HeapName}, _From, State) ->
    TimeHeap = erlang:get(HeapName),
    Ret = time_heap:top(TimeHeap#time_heap.array),
    {reply, Ret, State};

do_call(Req, _From, State) ->
    io:format("do_call unknown req: ~p~n", [Req]),
    {noreply, State}.

do_cast({apply, Mod, Func, Args}, State) ->
    erlang:apply(Mod, Func, Args),
    {noreply, State};

do_cast(Req, State) ->
    io:format("do_cast unknown req: ~p~n", [Req]),
    {noreply, State}.

do_info({apply, Mod, Func, Args}, State) ->
    erlang:apply(Mod, Func, Args),
    {noreply, State};

do_info({tick, HeapName}, State) ->
    time_heap:tick(HeapName),
    {noreply, State};

do_info(Req, State) ->
    io:format("do_info unknown req: ~p~n", [Req]),
    {noreply, State}.