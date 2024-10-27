-module(spectator_ffi).
-export([
    get_status/3,
    get_state/3,
    sys_suspend/2,
    sys_resume/2,
    list_processes/1,
    get_process_info/2,
    format_pid/1,
    get_details/2,
    format_port/1,
    list_ets_tables/1,
    get_ets_data/2,
    new_ets_table/2,
    get_word_size/1,
    opaque_tuple_to_list/1,
    get_ets_table_info/2,
    compare_data/2,
    list_ports/1,
    get_port_info/2,
    get_port_details/2,
    pid_to_string/1,
    port_to_string/1,
    pid_from_string/1,
    port_from_string/1,
    get_memory_statistics/1,
    get_system_info/1,
    truncate_float/1,
    kill_process/2,
    set_cookie/2
]).

% ---------------------------------------------------
% CALL HARNESS
% ---------------------------------------------------

-define(ERPC_TIMEOUT, 1000).

% Make a function to the local or remote node.
% NodeOption is a Gleam option, if passed as Some(node), the call is made via erpc to that node,
% otherwise it is made locally to the current node via erlang:apply/3.
do_call(NodeOption, Module, Function, Args) ->
    case NodeOption of
        none ->
            apply(Module, Function, Args);
        {some, NodeName} ->
            erpc:call(
                NodeName,
                Module,
                Function,
                Args,
                ?ERPC_TIMEOUT
            )
    end.

% Wrap in try/catch and transform into a Gleam result
to_result(Function) ->
    try
        {ok, Function()}
    catch
        % An error occurred with the ERPC call
        error:{erpc, Reason} -> {error, {erpc_error, Reason}};
        % Standard Erlang errors
        error:notsup -> {error, {not_supported_error}};
        error:badarg -> {error, {bad_argument_error}};
        % A function call unexpectedly returned a result of undefined
        % (we throw this error in nested functions to avoid returning undefined to Gleam)
        error:returned_undefined -> {error, returned_undefined_error};
        % Fallback for any other error
        _:Reason -> {error, {dynamic_error, Reason}}
    end.

% ---------------------------------------------------
% GENERAL HELPERS
% ---------------------------------------------------
% these are used inside ffi functions but not exported

% Normalize a pid, port, or nif resource into a SystemPrimitive() type
% also looks up the spectator tag for a pid if it is a process
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
        case do_call(none, erlang, process_info, [Pid, registered_name]) of
            {registered_name, Name} -> {some, Name};
            _ -> none
        end
    catch
        error:badarg -> none
    end.

% Get the name of a port wrapped in a Gleam Option type
get_port_name_option(Port) ->
    try
        case do_call(none, erlang, port_info, [Port, name]) of
            {registered_name, Name} -> {some, Name};
            _ -> none
        end
    catch
        error:badarg -> none
    end.

to_option(Input) ->
    case Input of
        [] -> none;
        undefined -> none;
        Value -> {some, Value}
    end.

% ---------------------------------------------------
% PROCESSES
% ---------------------------------------------------

% List all processes on a node
list_processes(NodeOption) ->
    to_result(fun() ->
        do_call(NodeOption, erlang, processes, [])
    end).

% Kill a process
% https://www.erlang.org/doc/apps/erts/erlang.html#exit/2
kill_process(NodeOption, Pid) ->
    to_result(fun() -> do_call(NodeOption, erlang, exit, [Pid, kill]) end).

% Get the status of an OTP-compatible process
% https://www.erlang.org/doc/apps/stdlib/sys.html#get_status/2
get_status(NodeOption, Name, Timeout) ->
    to_result(fun() ->
        {status, Pid, {module, Module}, SItems} = do_call(NodeOption, sys, get_status, [
            Name, Timeout
        ]),
        {SysState, Parent} = extract_sysstate_and_parent(SItems),
        {status, Pid, Module, Parent, SysState, SItems}
    end).

% Get the state of an OTP-compatible process or return an error
% https://www.erlang.org/doc/apps/stdlib/sys.html#get_state/2
get_state(NodeOption, Name, Timeout) ->
    to_result(fun() ->
        do_call(NodeOption, sys, get_state, [Name, Timeout])
    end).

% From a list of sys state items, extract the sys state and parent into a tuple
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

% 
sys_suspend(NodeOption, Pid) ->
    to_result(fun() -> do_call(NodeOption, sys, suspend, [Pid]) end).

sys_resume(NodeOption, Pid) ->
    to_result(fun() -> do_call(NodeOption, sys, resume, [Pid]) end).

% Throw an error if a value is undefined
% !! THROWS - wrap in to_result
assert_val(Value) ->
    case Value of
        undefined ->
            {
                error(returned_undefined)
            };
        Val ->
            Val
    end.

% Get the info of a regular process for display in a list
% https://www.erlang.org/doc/apps/erts/erlang.html#process_info/2
% Return is typed as ProcessInfo() in Gleam
get_process_info(NodeOption, Name) ->
    ItemList = [
        current_function,
        initial_call,
        registered_name,
        memory,
        message_queue_len,
        reductions,
        status
    ],
    to_result(fun() ->
        P = do_call(NodeOption, erlang, process_info, [Name, ItemList]),
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
                InfoNormalized
        end
    end).

% Get additional details of a process for display in a details view
get_details(NodeOption, Name) ->
    ItemList = [
        messages,
        links,
        monitored_by,
        monitors,
        trap_exit,
        parent
    ],
    to_result(fun() ->
        P = do_call(NodeOption, erlang, process_info, [Name, ItemList]),
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
                                    case do_call(NodeOption, erlang, whereis, [RegName]) of
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
                                    case do_call(NodeOption, erlang, whereis, [RegName]) of
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
                DetailsNormalized
        end
    end).

% ---------------------------------------------------
% PORTS
% ---------------------------------------------------

% Extract the second element from a two element tuple, thrwoing an error if it is undefined
% For use in decoding port info responses.
% !! THROWS - wrap in to_result
extract_val(Tuple) ->
    assert_val(element(2, Tuple)).


list_ports(NodeOption) ->
    to_result(fun() ->
        do_call(NodeOption, erlang, ports, [])
    end).

% Get the info of a port for display in a list
% https://www.erlang.org/doc/apps/erts/erlang.html#port_info/2
% Return is typed as PortInfo() in Gleam
get_port_info(NodeOption, Port) ->
    to_result(fun() ->
        {port_info,
            % Port name, this is the command that the port was started with
            list_to_bitstring(
                extract_val(
                    do_call(NodeOption, erlang, port_info, [Port, name])
                )
            ),
            % Registered name, if the port is registered
            to_option(
                do_call(NodeOption, erlang, port_info, [Port, registered_name])
            ),
            % Process connected to the port
            classify_system_primitive(
                extract_val(
                    do_call(NodeOption, erlang, port_info, [Port, connected])
                )
            ),
            % Operating system process ID
            to_option(
                element(
                    2,
                    do_call(NodeOption, erlang, port_info, [Port, os_pid])
                )
            ),
            % Input bytes
            extract_val(
                do_call(NodeOption, erlang, port_info, [Port, input])
            ),
            % Output bytes
            extract_val(
                do_call(NodeOption, erlang, port_info, [Port, output])
            ),
            % Memory used by the port
            extract_val(
                do_call(NodeOption, erlang, port_info, [Port, memory])
            ),
            % Queue size
            extract_val(
                do_call(NodeOption, erlang, port_info, [Port, queue_size])
            )}
    end).

% Get the details of a port for display in a details view
% https://www.erlang.org/doc/apps/erts/erlang.html#port_info/2
% Return is typed as PortDetails() in Gleam
get_port_details(NodeOption, Port) ->
    to_result(fun() ->
        {port_details,
            % Links -> we normalize into SystemPrimitive()
            lists:map(
                fun classify_system_primitive/1,
                extract_val(
                    do_call(NodeOption, erlang, port_info, [Port, links])
                )
            ),
            % Monitored By -> we normalize into SystemPrimitive()
            lists:map(
                fun classify_system_primitive/1,
                extract_val(
                    do_call(NodeOption, erlang, port_info, [Port, monitored_by])
                )
            ),
            % Monitors -> we normalize into SystemPrimitive()
            lists:map(
                fun classify_system_primitive/1,
                extract_val(
                    do_call(NodeOption, erlang, port_info, [Port, monitors])
                )
            )}
    end).

% ---------------------------------------------------
% ETS
% ---------------------------------------------------

% Construct a Table() type from an ETS table ID
% !! THROWS - wrap in to_result
build_table_info(NodeOption, Table) ->
    {table, assert_val(do_call(NodeOption, ets, info, [Table, id])),
        assert_val(do_call(NodeOption, ets, info, [Table, name])),
        assert_val(do_call(NodeOption, ets, info, [Table, type])),
        assert_val(do_call(NodeOption, ets, info, [Table, size])),
        assert_val(do_call(NodeOption, ets, info, [Table, memory])),
        classify_system_primitive(assert_val(do_call(NodeOption, ets, info, [Table, owner]))),
        assert_val(do_call(NodeOption, ets, info, [Table, protection])),
        assert_val(do_call(NodeOption, ets, info, [Table, read_concurrency])),
        assert_val(do_call(NodeOption, ets, info, [Table, write_concurrency]))}.

% Return a list of all ETS tables on the node,
% already populated with information, as Table() types.
list_ets_tables(NodeOption) ->
    to_result(fun() ->
        lists:map(
            fun(Table) -> build_table_info(NodeOption, Table) end,
            do_call(NodeOption, ets, all, [])
        )
    end).

% Return the info of a single ETS table as a Table() type
get_ets_table_info(NodeOption, Table) ->
    to_result(fun() ->
        case do_call(NodeOption, ets, info, [Table, id]) of
            undefined -> error(returned_undefined);
            TableId -> build_table_info(NodeOption, TableId)
        end
    end).

% Return the data of an ETS table
get_ets_data(NodeOption, Table) ->
    to_result(fun() ->
        do_call(NodeOption, ets, match, [Table, '$1'])
    end).

% Create a new ETS table
new_ets_table(NodeOption, Name) ->
    to_result(fun() ->
        do_call(NodeOption, ets, new, [Name, [named_table, public]])
    end).

% ---------------------------------------------------
% SYSTEM INFO
% ---------------------------------------------------

% Get system info, as a SystemInfo() type
get_system_info(NodeOption) ->
    to_result(fun() ->
        {
            system_info,
            % Uptime String
            uptime_string(NodeOption),
            % Architecture
            list_to_bitstring(
                do_call(NodeOption, erlang, system_info, [system_architecture])
            ),
            % ERTS version
            list_to_bitstring(
                do_call(NodeOption, erlang, system_info, [version])
            ),
            % OTP release
            list_to_bitstring(
                do_call(NodeOption, erlang, system_info, [otp_release])
            ),
            % Schedulers
            do_call(NodeOption, erlang, system_info, [schedulers]),
            % Schedulers online
            do_call(NodeOption, erlang, system_info, [schedulers_online]),
            % Atom count
            do_call(NodeOption, erlang, system_info, [atom_count]),
            % Atom limit
            do_call(NodeOption, erlang, system_info, [atom_limit]),
            % ETS count
            do_call(NodeOption, erlang, system_info, [ets_count]),
            % ETS limit
            do_call(NodeOption, erlang, system_info, [ets_limit]),
            % Port count
            do_call(NodeOption, erlang, system_info, [port_count]),
            % Port limit
            do_call(NodeOption, erlang, system_info, [port_limit]),
            % Process count
            do_call(NodeOption, erlang, system_info, [process_count]),
            % Process limit
            do_call(NodeOption, erlang, system_info, [process_limit])
        }
    end).

% Get memory stats
% as a MemoryStatistics() type
get_memory_statistics(NodeOption) ->
    to_result(fun() ->
        list_to_tuple([
            memory_statistics
            | element(
                2,
                lists:unzip(
                    do_call(NodeOption, erlang, memory, [])
                )
            )
        ])
    end).

get_word_size(NodeOption) ->
    to_result(fun() ->
        do_call(NodeOption, erlang, system_info, [wordsize])
    end).

% !! THROWS - wrap in to_result
uptime_seconds(NodeOption) ->
    NativeUptime =
        do_call(NodeOption, erlang, monotonic_time, []) -
            do_call(none, erlang, system_info, [start_time]),
    do_call(NodeOption, erlang, convert_time_unit, [NativeUptime, native, seconds]).

% !! THROWS - wrap in to_result
uptime_string(NodeOption) ->
    {D, {H, M, S}} =
        do_call(NodeOption, calendar, seconds_to_daystime, [uptime_seconds(NodeOption)]),
    list_to_bitstring(
        io_lib:format("~p days ~p hours ~p minutes ~p seconds", [D, H, M, S])
    ).

% ---------------------------------------------------
% LOCAL HELPERS
% ---------------------------------------------------
% (these are exported but are not related to node inspection)

pid_to_string(Pid) ->
    list_to_bitstring(pid_to_list(Pid)).

set_cookie(Node, Cookie) ->
  to_result(fun() -> erlang:set_cookie(Node, Cookie) end).

truncate_float(F) ->
    list_to_bitstring(io_lib:format("~.2f", [F])).

port_to_string(Port) ->
    list_to_bitstring(port_to_list(Port)).

pid_from_string(String) ->
    try
        {ok, list_to_pid(bitstring_to_list(String))}
    catch
        error:badarg -> {error, nil}
    end.

port_from_string(String) ->
    try
        {ok, list_to_port(bitstring_to_list(String))}
    catch
        error:badarg -> {error, nil}
    end.

format_pid(Pid) ->
    list_to_bitstring(pid_to_list(Pid)).

format_port(Port) ->
    list_to_bitstring(port_to_list(Port)).

compare_data(Data1, Data2) when Data1 < Data2 ->
    lt;
compare_data(Data1, Data2) when Data1 > Data2 ->
    gt;
compare_data(_Data1, _Data2) ->
    eq.

opaque_tuple_to_list(Tuple) ->
    tuple_to_list(Tuple).
