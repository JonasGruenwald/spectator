-module(carpenter_ffi).

-export([new_table/2, coerce/1]).

new_table(Name, Options) ->
  try
    {ok, ets:new(Name, Options)}
  catch
    error:badarg ->
      {error, nil}
  end.

coerce(A) ->
  A.
