-module(spectator_tag_manager).
-behaviour(gen_server).

-define(TABLE_NAME, spectator_process_tags).
-define(SERVER_NAME, spectator_tag_manager).

%% API
-export([start_link/0, add_tag/2, get_tag/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, terminate/2, code_change/3, handle_cast/2]).

%% API Functions
start_link() ->
    gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, [], []).

add_tag(Pid, Tag) when is_pid(Pid), is_binary(Tag) ->
    case whereis(?SERVER_NAME) of
        % server is not running, do nothing
        undefined -> ok;
        _ -> gen_server:call(?SERVER_NAME, {add_tag, Pid, Tag})
    end.

get_tag(Pid) when is_pid(Pid) ->
    try
        case ets:lookup(?TABLE_NAME, Pid) of
            [{_, Tag}] -> {some, Tag};
            _ -> none
        end
    catch
        _:_Reason -> none
    end.

%% gen_server callbacks
init([]) ->
    ets:new(?TABLE_NAME, [named_table, public, set]),
    {ok, undefined}.

handle_call({add_tag, Pid, Tag}, _From, _State) ->
    ets:insert(?TABLE_NAME, {Pid, Tag}),
    monitor(process, Pid),
    {reply, ok, undefined}.

handle_cast(_, State) -> {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, _State) ->
    ets:delete(?TABLE_NAME, Pid),
    {noreply, undefined}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
