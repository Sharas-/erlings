-module(rpcc).
-export([rpc/2, send/2]).

rpc(undefined, _)->
	throw("no process registered by that name");
rpc(RegName, Msg) when is_atom(RegName) ->
	rpc(whereis(RegName), Msg);
rpc(Pid, Msg) when is_pid(Pid) ->
	send(Pid, Msg),
	receive {Pid, Ans} -> Ans 
	after 2000 -> throw(timeout)
	end.

send(Pid, Msg)->
	Pid ! {self(), Msg}.
