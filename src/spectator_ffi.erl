-module(spectator_ffi).
-export([get_status/2, get_state/2, get_info/1, get_all_info/1, format_pid/1]).

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
        {ok, sys:get_state(Name, Timeout)}
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
        P = erlang:process_info(Name, ItemList),
        % erlang:display(P),
        case P of
            undefined ->
                {error, not_found};
            [] ->
                {error, no_info};
            Info ->
                % erlang:display(Info),
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
                        RegisteredName -> {some, RegisteredName}
                    end,
                    element(4, InfoTuple),
                    element(5, InfoTuple),
                    element(6, InfoTuple),
                    % Convert tag to option type
                    case element(7, InfoTuple) of
                        undefined -> none;
                        SpectatorDebugTag -> {some, SpectatorDebugTag}
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

format_pid(Pid) ->
    list_to_bitstring(pid_to_list(Pid)).
