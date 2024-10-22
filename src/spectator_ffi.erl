-module(spectator_ffi).
-export([
    get_status/2,
    get_state/2,
    get_process_info/1,
    format_pid/1,
    get_details/1,
    format_port/1,
    list_ets_tables/0,
    get_ets_data/1,
    new_ets_table/1,
    get_word_size/0,
    opaque_tuple_to_list/1,
    get_ets_table_info/1,
    compare_data/2,
    get_port_info/1,
    get_port_details/1
]).

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
        running -> extract_sysstate_and_parent(T, process_running, Parent);
        suspended -> extract_sysstate_and_parent(T, process_suspended, Parent);
        Pid when is_pid(Pid) -> extract_sysstate_and_parent(T, SysState, Pid);
        _ -> extract_sysstate_and_parent(T, SysState, Parent)
    end.

% Get the info of a regular process for display in a list
get_process_info(Name) ->
    ItemList = [
        current_function,
        initial_call,
        registered_name,
        memory,
        message_queue_len,
        reductions,
        status
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
                    process_info,
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
                    spectator_tag_manager:get_tag(Name),
                    element(7, InfoTuple)
                },
                {ok, InfoNormalized}
        end
    catch
        _:Reason -> {error, Reason}
    end.

% Get the info of a port for display in a list
get_port_info(Port) ->
    try
        % TODO would be nice to check if any of these are undefined first
        {ok,
            {port_info, list_to_bitstring(element(2, erlang:port_info(Port, name))),
                to_option(erlang:port_info(Port, registered_name)),
                element(2, erlang:port_info(Port, connected)),
                element(2, erlang:port_info(Port, os_pid)),
                element(2, erlang:port_info(Port, input)),
                element(2, erlang:port_info(Port, output)),
                element(2, erlang:port_info(Port, memory)),
                element(2, erlang:port_info(Port, queue_size))}}
    catch
        _:Reason -> {error, Reason}
    end.

get_port_details(Port) ->
    try
        % TODO would be nice to check if any of these are undefined first
        {ok,
            {port_details,
                % Links -> we normalize into SystemPrimitive()
                lists:map(
                    fun classify_system_primitive/1,
                    element(2, erlang:port_info(Port, links))
                ),
                % Monitored By -> we normalize into SystemPrimitive()
                lists:map(
                    fun classify_system_primitive/1,
                    element(2, erlang:port_info(Port, monitored_by))
                ),
                % Monitors -> we normalize into SystemPrimitive()
                lists:map(
                    fun classify_system_primitive/1,
                    element(2, erlang:port_info(Port, monitors))
                )}}
    catch
        _:Reason -> {error, Reason}
    end.

% Normalize a pid, port, or nif resource into a SystemPrimitive() type
classify_system_primitive(Item) ->
    case Item of
        Process when is_pid(Process) ->
            {process_primitive, Process, get_process_name_option(Process),
                spectator_tag_manager:get_tag(Process)};
        Port when is_port(Port) -> {port_primitive, Port, get_port_name_option(Port)};
        NifResource ->
            {nif_resource_primitive, NifResource}
    end.

% Get the name of a process wrapped in a Gleam Option type
get_process_name_option(Pid) ->
    try
        case erlang:process_info(Pid, registered_name) of
            {registered_name, Name} -> {some, Name};
            _ -> none
        end
    catch
        error:badarg -> none
    end.

% Get the name of a port wrapped in a Gleam Option type
get_port_name_option(Port) ->
    try
        case erlang:port_info(Port, name) of
            {registered_name, Name} -> {some, Name};
            _ -> none
        end
    catch
        error:badarg -> none
    end.

% Get additional details of a process for display in a details view
get_details(Name) ->
    ItemList = [
        messages,
        links,
        monitored_by,
        monitors,
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
                    % Messages
                    element(1, DetailsTuple),
                    % Links -> we normalize into SystemPrimitive()
                    lists:map(
                        fun classify_system_primitive/1,
                        element(2, DetailsTuple)
                    ),
                    % Monitored By -> we normalize into SystemPrimitive()
                    lists:map(
                        fun classify_system_primitive/1,
                        element(3, DetailsTuple)
                    ),
                    % Monitors -> we normalize into SystemPrimitive()
                    % There is a lot of remapping here, let's break it down:
                    % - We receive the monitors either by id or name based on how they are monitored
                    % - But we don't actually care if a resource is monitored by id or name
                    %    - We DO want to know the id of every resource, even if its is monitored by name
                    %    - We also want to know the name of the resource if it has one,
                    %      even if it is not monitored by that name
                    % - For this reason we look up the respective id/name for each resource.
                    % - We also handle the special case of a remote process monitored by name separately.
                    % - In case an ID lookup fails, we filter out the resource.
                    lists:filtermap(
                        fun(Item) ->
                            case Item of
                                % Local process monitored by name
                                {process, {RegName, Node}} when Node == node() ->
                                    case whereis(RegName) of
                                        % If the PID lookup fails, we filter this process out
                                        undefined ->
                                            false;
                                        Pid ->
                                            {true,
                                                {process_primitive, Pid, {some, RegName},
                                                    spectator_tag_manager:get_tag(Pid)}}
                                    end;
                                %  Remote process monitored by name
                                {process, {RegName, Node}} ->
                                    {true, {remote_process_primitive, RegName, Node}};
                                % Local process monitored by pid
                                {process, Pid} ->
                                    {true,
                                        {process_primitive, Pid, get_process_name_option(Pid),
                                            spectator_tag_manager:get_tag(Pid)}};
                                % Port monitored by name
                                % (Node is always the local node, it's a legacy field)
                                {port, {RegName, _Node}} ->
                                    case whereis(RegName) of
                                        % If the Port ID lookup fails, we filter this port out
                                        undefined -> false;
                                        Port -> {true, {port_primitive, Port, RegName}}
                                    end;
                                % Port monitored by port id
                                {port, PortId} ->
                                    {true, {port_primitive, PortId, get_port_name_option(PortId)}}
                            end
                        end,
                        element(4, DetailsTuple)
                    ),
                    % Trap Exit
                    element(5, DetailsTuple),
                    % Parent
                    case element(6, DetailsTuple) of
                        undefined -> none;
                        Parent -> {some, classify_system_primitive(Parent)}
                    end
                },
                {ok, DetailsNormalized}
        end
    catch
        E:Reason ->
            erlang:display(E),
            {error, Reason}
    end.

format_pid(Pid) ->
    list_to_bitstring(pid_to_list(Pid)).

format_port(Port) ->
    list_to_bitstring(port_to_list(Port)).

build_table_info(Table) ->
    % TODO would be nice to check if any of these are undefined first
    {table, ets:info(Table, id), ets:info(Table, name), ets:info(Table, type),
        ets:info(Table, size), ets:info(Table, memory), ets:info(Table, owner),
        ets:info(Table, protection), ets:info(Table, read_concurrency),
        ets:info(Table, write_concurrency)}.

list_ets_tables() ->
    try
        {ok,
            lists:map(
                fun build_table_info/1,
                ets:all()
            )}
    catch
        error:badarg -> {error, nil}
    end.

get_ets_table_info(Table) ->
    try
        case ets:info(Table, id) of
            undefined -> {error, nil};
            TableId -> {ok, build_table_info(TableId)}
        end
    catch
        error:badarg -> {error, nil}
    end.

compare_data(Data1, Data2) when Data1 < Data2 ->
    lt;
compare_data(Data1, Data2) when Data1 > Data2 ->
    gt;
compare_data(_Data1, _Data2) ->
    eq.

get_ets_data(Table) ->
    try
        {ok, ets:match(Table, '$1')}
    catch
        error:badarg -> {error, nil}
    end.

opaque_tuple_to_list(Tuple) ->
    tuple_to_list(Tuple).

new_ets_table(Name) ->
    try
        {ok,
            ets:new(Name, [
                named_table, public
            ])}
    catch
        error:badarg -> {error, nil}
    end.

get_word_size() ->
    erlang:system_info(wordsize).

to_option(Input) ->
    case Input of
        [] -> none;
        undefined -> none;
        Value -> {some, Value}
    end.
