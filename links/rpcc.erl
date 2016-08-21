-module(rpcc).
-export([call/2, send/2]).

call(undefined, _)->
	throw("no process registered by that name");
call(RegName, Msg) when is_atom(RegName) ->
	call(whereis(RegName), Msg);
call(Pid, Msg) when is_pid(Pid) ->
	send(Pid, Msg),
	receive {Pid, Ans} -> Ans 
	after 2000 -> throw(timeout)
	end.

send(Pid, Msg)->
	Pid ! {self(), Msg}.
