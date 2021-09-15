%%% -----------------------------------------------------------
%%% @author eva
%%% @date 2021.09.15
%%% @doc 测试模块
%%% @end
%%% -----------------------------------------------------------
-module(test).

-include("time_heap.hrl").

-export([init_timer/0, add_timer/3, del_timer/1, top/0]).
-export([cb_func/1]).

init_timer() ->
    serv_test:cast({apply, time_heap, init_time_heap, [time_heap]}),
    ok.

add_timer(Id, Duration, LoopInterval) ->
    NowTime = util:now_ms(),
    Timer = time_heap:new_timer(NowTime + Duration, {test, cb_func, [Id]}, LoopInterval),
    CostTime = serv_test:call({add_timer, time_heap, Timer}),
    {CostTime, Timer#heap_timer.ref}.

del_timer(Ref) ->
    serv_test:cast({apply, time_heap, del_timer, [time_heap, Ref]}),
    ok.

top() ->
    serv_test:call({top, time_heap}).

%% 回调函数
cb_func(Id) ->
    io:format("cb_func: ~p~n", [Id]),
    ok.