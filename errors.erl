-module(errors).
-export([start_system_process/0, on_exit/2]).


start_system_process()->
spawn(fun()->
	process_flag(trap_exit, true), 
	loop()
      end).

loop()->
receive {'EXIT', Pid, Reason}-> 
		io:format("got exit signal from ~p, reason: ~p", [Pid, Reason]), 
		loop(); 
	{From, ping} -> 
		From ! {self(), pong}, 
		loop() 
end.

on_exit(P, F)->
	spawn(fun()->
		Mon = monitor(process, P),	
		receive {'DOWN', Mon, process, P, Reason} ->
			F(Reason)
		end
	      end).



