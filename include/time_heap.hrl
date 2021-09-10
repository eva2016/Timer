-ifndef(TIME_HEAP_HRL).
-define(TIME_HEAP_HRL, true).

%% 定时器类
-record(heap_timer, {
    ref,                    %% 唯一标识
    expire,                 %% 定时器生效绝对时间(ms)
    cb_func = none,         %% 定时器回调函数: {mod, func, args} | none
    loop_interval = 0       %% 循环时间间隔(ms): 0-不循环
    }).

%% 时间堆
-record(time_heap, {
    array,                  %% 堆
    ref_idxs,               %% 定时器坐标索引
    timer_ref = none        %% 堆顶定时器索引
    }).

-endif.