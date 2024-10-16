-module(spectator_ffi).
-export([get_status/2, get_state/2, get_info/1, get_all_info/1]).

get_status(Name, Timeout) ->
    try
        {status, Pid, {module, Module}, SItems} = sys:get_status(Name, Timeout),
        {SysState, Parent} = extract_sysstate_and_parent(SItems),
        {ok, {status, Pid, Module, Parent, SysState, SItems}}
    catch
        _:Reason -> {error, Reason}
    end.

get_state(Name, Timeout) ->
    try
        case sys:get_state(Name, Timeout) of
            {ok, State} -> {ok, State};
            State -> {ok, State}
        end
    catch
        _:Reason -> {error, Reason}
    end.

extract_sysstate_and_parent(SItems) ->
    extract_sysstate_and_parent(SItems, undefined, undefined).

extract_sysstate_and_parent([], SysState, Parent) ->
    {SysState, Parent};
extract_sysstate_and_parent([H | T], SysState, Parent) ->
    case H of
        running -> extract_sysstate_and_parent(T, running, Parent);
        suspended -> extract_sysstate_and_parent(T, suspended, Parent);
        Pid when is_pid(Pid) -> extract_sysstate_and_parent(T, SysState, Pid);
        _ -> extract_sysstate_and_parent(T, SysState, Parent)
    end.

get_info(Name) ->
    ItemList = [
        current_function,
        initial_call,
        registered_name,
        memory,
        message_queue_len,
        reductions,
        {dictionary, spectator_debug_tag}
    ],
    try
        case erlang:process_info(Name, ItemList) of
            undefined ->
                {error, not_found};
            [] ->
                {error, no_info};
            Info ->
                {_Keys, Values} = lists:unzip(Info),
                InfoTuple = list_to_tuple(Values),
                InfoNormalized = {
                    % Prefix to turn into Info() type
                    info,
                    element(1, InfoTuple),
                    element(2, InfoTuple),
                    % Convert registered name to option type
                    case element(3, InfoTuple) of
                        [] -> none;
                        Name -> {some, Name}
                    end,
                    element(4, InfoTuple),
                    element(5, InfoTuple),
                    element(6, InfoTuple),
                    % Search in process dictionary for tag
                    case element(7, InfoTuple) of
                        undefined -> none;
                        Value -> {some, Value}
                    end
                },
                {ok, InfoNormalized}
        end
    catch
        _:Reason -> {error, Reason}
    end.

get_all_info(Name) ->
    case erlang:process_info(Name) of
        undefined -> {error, not_found};
        [] -> {error, no_info};
        Info -> {ok, Info}
    end.
