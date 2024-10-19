-module(spectator_ffi).
-export([get_status/2, get_state/2, get_info/1, get_all_info/1, format_pid/1, get_details/1, format_port/1]).

% Get the status of an OTP-compatible process or return an error
get_status(Name, Timeout) ->
    try
        {status, Pid, {module, Module}, SItems} = sys:get_status(Name, Timeout),
        {SysState, Parent} = extract_sysstate_and_parent(SItems),
        {ok, {status, Pid, Module, Parent, SysState, SItems}}
    catch
        _:Reason -> {error, Reason}
    end.

% Get the state of an OTP-compatible process or return an error
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

% Get the info of a regular process for display in a list
get_info(Name) ->
    ItemList = [
        current_function,
        initial_call,
        registered_name,
        memory,
        message_queue_len,
        reductions
    ],
    try
        P = erlang:process_info(Name, ItemList),
        case P of
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
                        RegisteredName -> {some, RegisteredName}
                    end,
                    element(4, InfoTuple),
                    element(5, InfoTuple),
                    element(6, InfoTuple),
                    spectator_tag_manager:get_tag(Name)
                },
                {ok, InfoNormalized}
        end
    catch
        _:Reason -> {error, Reason}
    end.

classify_system_primitive(Item) ->
    case Item of
        Process when is_pid(Process) -> {process, Process};
        Port when is_port(Port) -> {port, Port};
        NifResource -> {nif_resource, NifResource}
    end.

% Get additional details of a process for display in a details view
get_details(Name) ->
    ItemList = [
        messages,
        links,
        monitored_by,
        monitors,
        status,
        trap_exit,
        parent
    ],
    try
        P = erlang:process_info(Name, ItemList),
        case P of
            undefined ->
                {error, not_found};
            [] ->
                {error, no_info};
            Info ->
                {_Keys, Values} = lists:unzip(Info),
                DetailsTuple = list_to_tuple(Values),
                DetailsNormalized = {
                    % Prefix to turn into Details() type
                    details,
                    element(1, DetailsTuple),
                    lists:map(
                        fun classify_system_primitive/1,
                        element(2, DetailsTuple)
                    ),
                    lists:map(
                        fun classify_system_primitive/1,
                        element(2, DetailsTuple)
                    ),
                    lists:map(
                        fun(Item) ->
                            case Item of
                                {process, {RegName, _Node}} ->
                                    {registered_process, RegName};
                                {process, Pid} ->
                                    {process, Pid};
                                {port, {RegName, _Node}} ->
                                    {registered_port, RegName};
                                {port, PortId} ->
                                    {port, PortId}
                            end
                        end,
                        element(4, DetailsTuple)
                    ),
                    element(5, DetailsTuple),
                    element(6, DetailsTuple),
                    case element(7, DetailsTuple) of
                        undefined -> none;
                        Parent -> {some, Parent}
                    end
                },
                {ok, DetailsNormalized}
        end
    catch
        E:Reason ->
            erlang:display(E),
            {error, Reason}
    end.

get_all_info(Name) ->
    case erlang:process_info(Name) of
        undefined -> {error, not_found};
        [] -> {error, no_info};
        Info -> {ok, Info}
    end.

format_pid(Pid) ->
    list_to_bitstring(pid_to_list(Pid)).

format_port(Port) ->
    list_to_bitstring(port_to_list(Port)).


