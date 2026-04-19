-module(spectator_test_ffi).
-export([new_ets_table/2]).

new_ets_table(Name, Options) ->
    try
        {ok, ets:new(Name, Options)}
    catch
        error:badarg -> {error, nil};
        _:Reason -> {error, Reason}
    end.