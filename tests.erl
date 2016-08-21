-module(tests).

-compile(export_all).

worker()->
	receive
	after infinity
	      ok
	end.


