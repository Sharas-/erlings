-module(yafs).
-export([start/0, get_file/2]).

start()->
	spawn_link(fun loop/0).

get_file(SrvPid, File)->
	SrvPid ! {self(), get_file, File},
	receive M -> 
			M
	after 300 -> 
		      timeout
	end.

loop()->
	receive {From, get_file, File} ->
			From ! file:read_file(File),
			loop()
	end.

