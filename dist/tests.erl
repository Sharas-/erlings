-module(tests).

-compile(export_all).

-define(other_node, 'servas@sharas').

start_server()->
	spawn_link(fun loop/0).

loop()->
receive {From, ping} ->
		io:format("received ping from ~p~n", [From]),
		From ! pong,
		loop();
	{From, Fun} when is_function(Fun) ->
		io:format("executing function ~p received from ~p~n", [Fun, From]),
		From ! Fun(),
		loop()
end.

load_module_on_node(Node, ModName)when is_atom(ModName)->
	{ok, ModName, Binary} = compile:file(ModName, [verbose,report_errors,report_warnings, binary]),
	{module, ModName} = rpc:call(Node, code, load_binary, [ModName, atom_to_list(ModName), Binary]).

%yes it can, but module where clojure is defined (in this case "tests") must be loadable on remote node, and be of the same version! 
%only reference to the clusure will be sent to remote node after inlining it into the module. Code implementation is not sent.
%If closures' reference includes source modules' hashcode it will be compared on remote node to make sure modules match.
test_remote_node_can_execute_sent_clojure()->
	load_module_on_node(?other_node, funserver),
	load_module_on_node(?other_node, ?MODULE),
	Pid = spawn(?other_node, funserver, loop, []),
	OutVar = {"token with love from", node()},
	Pid ! {self(), fun()-> {OutVar, erlang:node()} end},
	receive Result -> 
		Result = {OutVar, node(Pid)}
	after 300 ->
		      timeout
	end.

test_can_monitor_remote_node()->
	net_kernel:monitor_nodes(true),
	monitor_node(?other_node, true),
	spawn(fun()-> disconnect_node(?other_node), timer:sleep(200), net_adm:ping(?other_node) end),
	ok = receive {nodedown, ?other_node} ->
			io:format("got nodedown~n"),
			ok
	after 1000 ->
		      timeout
	end,
	ok = receive {nodeup, ?other_node} ->
			io:format("got nodeup~n"),
			ok
	after 1000 ->
		      timeout
	end.

test_can_send_to_registered_name_on_node()->
	RegName = call_me,
	Pid = spawn(?other_node, fun()-> register(RegName, self()), receive {From, ping} -> 
										     io:format("~p received call from ~p~n", [self(), From]),
										     From ! {self(), pong} 
								     end
				  end),
	{RegName, ?other_node} ! ping,
	ok = receive {Pid, pong} ->
			ok
	after 300 ->
		      timeout
	end.

test_file_gets_sent_from_other_node()->
	case rpc:call(?other_node, yafs, start, []) of 
		{badrpc, Reason} ->
			io:format("can't call ~p, reason: ~p", [?other_node, Reason]);
		Pid when is_pid(Pid) ->
			{ok, File} = yafs:get_file(Pid, "/home/sharas/.ssh/config"),
			File
	end.
