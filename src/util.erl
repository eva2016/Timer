%%% -----------------------------------------------------------
%%% @author eva
%%% @date 2021.09.10
%%% @doc 通用函数库
%%% @end
%%% -----------------------------------------------------------
-module(util).

-export([now_ms/0, update_record/2]).

now_ms() -> 0.

update_record(Record, []) -> Record;
update_record(Record, [{Index, Value} | Rest]) ->
    NewRecord = erlang:setelement(Index, Record, Value),
    update_record(NewRecord, Rest).