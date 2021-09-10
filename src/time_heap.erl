%%% -----------------------------------------------------------
%%% @author eva
%%% @date 2021.09.09
%%% @doc 时间堆
%%% @end
%%% -----------------------------------------------------------
-module(time_heap).

-include("time_heap.hrl").

-export([init_time_heap/1, init_time_heap/2, add_timer/2, del_timer/2, tick/1]).
-export([new_timer/3]).

%% 初始化时间堆
%% @return: ok
init_time_heap(HeapName) ->
    TimeHeap = #time_heap{
        array = array:new(),
        ref_idxs = dict:new()
    },
    erlang:put(HeapName, TimeHeap),
    ok.

init_time_heap(HeapName, []) ->
    init_time_heap(HeapName);
init_time_heap(HeapName, TimerList) ->
    F1 = fun(Timer, {Index, Array, RefDict}) ->
            Array1 = array:set(Index, Array, Timer),
            RefDict1 = dict:store(Timer#heap_timer.ref, Index, RefDict),
            {Index+1, Array1, RefDict1}
        end,
    {CurSize, NewArray, NewRefDict} = lists:foldl(F1, {0, array:new(), dict:new()}, TimerList),
    %% 排序成为最小堆
    {NewArray1, NewRefDict1} = init_min_heap((CurSize-1) div 2, CurSize, NewArray, NewRefDict),
    #heap_timer{expire = ExpireTime} = top(NewArray1),
    TimerRef = active_top_timer(HeapName, ExpireTime),
    TimeHeap = #time_heap{
        array = NewArray1,
        ref_idxs = NewRefDict1,
        timer_ref = TimerRef
    },
    erlang:put(HeapName, TimeHeap),
    ok.

%% 初始化最小堆
%% @return: {Array, RefDict}
init_min_heap(Hole, Size, Array, RefDict) when Hole >= 0 ->
    Timer = array:get(Hole, Array),
    {NewArray, NewRefDict} = do_shift_down(Hole, Timer, Size, Array, RefDict),
    init_min_heap(Hole-1, Size, NewArray, NewRefDict);
init_min_heap(_Hole, _Size, Array, RefDict) ->
    {Array, RefDict}.

%% 添加目标定时器
%% @return: ok | fail
add_timer(HeapName, Timer) ->
    TimeHeap = erlang:get(HeapName),
    #time_heap{
        array = Array,
        ref_idxs = RefDict,
        timer_ref = TimerRef
    } = TimeHeap,
    #heap_timer{
        ref = Ref,
        expire = ExpireTime
    } = Timer,
    case dict:find(Ref, RefDict) of
        error ->
            Hole = array:size(Array),
            NewArray = array:set(Hole, Timer, TimeHeap),
            NewRefDict = dict:store(Timer#heap_timer.ref, Hole, RefDict),
            {Index, NewArray1, NewRefDict1} = do_shift_up(Hole, Timer, NewArray, NewRefDict),
            ItemList = [{#time_heap.array, NewArray1}, {#time_heap.ref_idxs, NewRefDict1}],
            case Index =:= 0 of
                true ->         %% 更换堆顶
                    erlang:cancel_timer(TimerRef),
                    NewTimerRef = active_top_timer(HeapName, ExpireTime),
                    ItemList1 = [{#time_heap.timer_ref, NewTimerRef} | ItemList];
                _ ->
                    ItemList1 = ItemList
            end,
            NewTimeHeap = util:update_record(TimeHeap, ItemList1),
            erlang:put(HeapName, NewTimeHeap),
            ok;
        {ok, Index} ->
            io_lib:format("add_timer error: ref exist ~p", [{HeapName, Ref, Index}]),
            fail
    end.

%% 删除目标定时器
%% @return: ok
del_timer(HeapName, Ref) ->
    TimeHeap = erlang:get(HeapName),
    #time_heap{
        array = Array,
        ref_idxs = RefDict
    } = TimeHeap,
    case dict:find(Ref, RefDict) of
        error -> ok;
        {ok, Index} ->
            Timer = array:get(Index, Array),
            NewArray = array:set(Index, Array, Timer#heap_timer{cb_func = none}),
            NewTimeHeap = TimeHeap#time_heap{array = NewArray},
            erlang:put(HeapName, NewTimeHeap)
    end.

%% 心博函数
tick(HeapName) ->
    TimeHeap = erlang:get(HeapName),
    #time_heap{
        array = Array,
        ref_idxs = RefDict
    } = TimeHeap,
    case array:size(Array) =:= 0 of
        true -> ok;
        _ ->
            NowTime = util:now_ms(),
            {NewArray, NewRefDict} = do_tick(NowTime, Array, RefDict),
            ItemList = [{#time_heap.array, NewArray}, {#time_heap.ref_idxs, NewRefDict}],
            case top(NewArray) of
                none ->
                    ItemList1 = ItemList;
                #heap_timer{expire = ExpireTime} ->
                    TimerRef = active_top_timer(HeapName, ExpireTime),
                    ItemList1 = [{#time_heap.timer_ref, TimerRef} | ItemList]
            end,
            NewTimeHeap = util:update_record(TimeHeap, ItemList1),
            erlang:put(HeapName, NewTimeHeap),
            ok
    end.

%% @return: {Array, RefDict}
do_tick(NowTime, Array, RefDict) ->
    case top(Array) of
        none ->
            {Array, RefDict};
        Timer ->
            #heap_timer{
                ref = TopRef,
                expire = ExpireTime,
                cb_func = FuncTuple,
                loop_interval = LoopInterval
            } = Timer,
            case NowTime >= ExpireTime orelse FuncTuple =:= none of
                true ->
                    {NewArray, NewRefDict} =
                        case FuncTuple of
                            none ->                 %% 定时器已被删除
                                do_pop_timer(TopRef, Array, RefDict);
                            {Mod, Func, Args} ->
                                erlang:apply(Mod, Func, Args),
                                case LoopInterval > 0 of
                                    true ->         %% 定时执行函数
                                        CurSize = array:size(Array),
                                        NewTimer = Timer#heap_timer{expire = ExpireTime+LoopInterval},
                                        do_shift_down(0, NewTimer, CurSize, Array, RefDict);
                                    _ ->
                                        do_pop_timer(TopRef, Array, RefDict)
                                end
                        end,
                    do_tick(NowTime, NewArray, NewRefDict);
                _ ->
                    {Array, RefDict}
            end
    end.

%% 启动堆顶的定时器
%% @return: TimerRef
active_top_timer(HeapName, EndTime) ->
    NowTime = util:now_ms(),
    erlang:send_after(max(EndTime-NowTime, 500), self(), {tick, HeapName}).

%% 获取堆顶的定时器
%% @return: HeapTimer | none
top(Array) ->
    case array:size(Array) =:= 0 of
        true -> none;
        _ -> array:get(0, Array)
    end.

%% 删除堆顶的定时器
%% @return: {Array, RefDict}
do_pop_timer(TopRef, Array, RefDict) ->
    CurSize = array:size(),
    case CurSize of
        0 ->
            {Array, RefDict};
        1 ->
            NewArray = array:reset(0, Array),
            NewArray1 = array:resize(NewArray),
            NewRefDict = dict:erase(TopRef, RefDict),
            {NewArray1, NewRefDict};
        _ ->
            LastTimer = array:get(CurSize-1, Array),
            NewArray = array:set(0, LastTimer, Array),
            NewRefDict = dict:erase(TopRef, RefDict),
            NewArray1 = array:reset(CurSize-1, NewArray),
            NewArray2 = array:resize(NewArray1),
            do_shift_down(0, LastTimer, CurSize-1, NewArray2, NewRefDict)
    end.

%% 最小堆的上滤操作
%% @return: {Hole, Array, RefDict}
do_shift_up(Hole, Timer, Array, RefDict) when Hole > 0 ->
    Parent = (Hole-1) div 2,
    PreTimer = array:get(Parent, Array),
    case PreTimer#heap_timer.expire =< Timer#heap_timer.expire of
        true ->
            NewArray = array:set(Hole, Timer, Array),
            NewRefDict = dict:store(Timer#heap_timer.ref, Hole, RefDict),
            {Hole, NewArray, NewRefDict};
        _ ->
            NewArray = array:set(Hole, PreTimer, Array),
            NewRefDict = dict:store(PreTimer#heap_timer.ref, Hole, RefDict),
            do_shift_up(Parent, Timer, NewArray, NewRefDict)
    end;
do_shift_up(Hole, Timer, Array, RefDict) ->
    NewArray = array:set(Hole, Timer, Array),
    NewRefDict = dict:store(Timer#heap_timer.ref, Hole, RefDict),
    {Hole, NewArray, NewRefDict}.

%% 最小堆的下滤操作
%% @return: {Array, RefDict}
do_shift_down(Hole, Timer, CurSize, Array, RefDict) when (Hole*2+1) =< (CurSize-1) ->
    LeftChild = Hole*2+1,
    LeftTimer = array:get(LeftChild, Array),
    {NextChild, NextTimer} = 
        case LeftChild < CurSize-1 of
            true ->
                RightChild = LeftChild+1,
                RightTimer = array:get(RightChild, Array),
                case LeftTimer#heap_timer.expire < RightChild#heap_timer.expire of
                    true -> {LeftChild, LeftTimer};
                    _ -> {RightChild, RightTimer}
                end;
            _ -> {LeftChild, LeftTimer}
        end,
    case NextTimer#heap_timer.expire < Timer#heap_timer.expire of
        true ->
            NewArray = array:set(Hole, NextTimer, Array),
            NewRefDict = dict:store(NextTimer#heap_timer.ref, Hole, RefDict),
            do_shift_down(NextChild, Timer, CurSize, NewArray, NewRefDict);
        _ ->
            NewArray = array:set(Hole, Timer, Array),
            NewRefDict = dict:store(Timer#heap_timer.ref, Hole, RefDict),
            {NewArray, NewRefDict}
    end;
do_shift_down(Hole, Timer, _CurSize, Array, RefDict) ->
    NewArray = array:set(Hole, Timer, Array),
    NewRefDict = dict:store(Timer#heap_timer.ref, Hole, RefDict),
    {NewArray, NewRefDict}.

%% @return: #heap_timer{}
new_timer(ExpireTime, FuncTuple, LoopInterval) ->
    #heap_timer{
        ref = erlang:make_ref(),
        expire = ExpireTime,
        cb_func = FuncTuple,
        loop_interval = LoopInterval
    }.