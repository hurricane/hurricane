-module(hurricane_utils).

-export([get_best_pid/1]).

get_best_pid(Name) ->
    Members = lists:map(fun(Pid) ->
        [
            {message_queue_len, Messages},
            {stack_size, StackSize}
        ] = erlang:process_info(Pid, [message_queue_len, stack_size]),
        {Pid, Messages, StackSize}
    end, pg2:get_members(Name)),
    SortedMembers = lists:keysort(2, lists:keysort(3, Members)),
    case SortedMembers of
        [{Pid, _Messages, _StackSize}] -> Pid;
        [{Pid, _Messages, _StackSize}, _Tail] -> Pid;
        _ -> {error, empty_process_group}
    end.
