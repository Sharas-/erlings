-module(funserver).

-compile(export_all).

loop()->
receive {From, ping} ->
		io:format("received ping from ~p~n", [From]),
		From ! pong,
		loop();
	{From, Fun} when is_function(Fun) ->
		error_logger:info_msg("executing function ~p received from ~p~n", [Fun, From]),
		From ! Fun(),
		loop();
	M -> 
		error_logger:error_msg("received ~p, don't know what to do with it", [M])
end.
