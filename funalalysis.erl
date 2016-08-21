-module(funanalysis).

-compile(export_all).

names_in_mods(ModFuns) ->
	funs:pivot(ModFuns).

most_used_name(ModFuns)->
	maps:fold(fun(K, V, {_, Mods}=Acc) -> if V > Mods -> {K, V}; true -> Acc end end, {x, []}, names_in_mods(ModFuns)).

