-module(funs).
-compile(export_all).

-export([filter1/2, filter2/2, max/2, tuple_to_list/1, t_seq/3, list/1, tuple_to_list2/1]).

filter1(P, L) ->
	[X || X<-L, P(X)].

filter2(P, [H|T]) ->
	case P(H) of
		true -> [H | filter2(P,T)];
		false -> filter2(P, T)
	end;
filter2(_, []) -> [].

max(X, N) when X > N -> X;
max(_, N) -> N.

tuple_to_list(T)->
	list(tuple_seq(T)).

tuple_to_list2({}) -> [];
tuple_to_list2(T) when is_tuple(T) ->
	lists:map(fun(I)-> element(I, T) end, lists:seq(1, size(T))).

  
tuple_seq({}) -> [];
tuple_seq(T) when is_tuple(T) ->
	t_seq(T, 1, size(T)+1).

t_seq(_, I, I) -> done;
t_seq(T, I, N) ->
	{element(I, T), fun()-> t_seq(T, I+1, N) end}.

list({V, I})->
	[V | list(I())];
list(done) -> [].

reverse_bin(<<>>) ->
	io:format("~w", ["called"]),
	<<0:0>>;
reverse_bin(<<H:1/binary, T/binary>>) ->
	io:format("~p ~p", [H,T]),
	<<H,(reverse_bin(T))>>.	
	  
pivot(Table) ->
	lists:foldl(fun pivot_row/2, #{}, Table).

pivot_row({_, []}, Map) ->
	Map;
pivot_row({Col, [H|T]}, Map) ->
	case Map of
		#{H := Row} -> pivot_row({Col, T}, Map#{H := [Col|Row]});
		_ -> pivot_row({Col, T}, Map#{H => [Col]})
	end.

%max_item(MaxFun, [H|T]) ->
%	lists:foldl(MaxFun, H, T).
max_item(MaxFun, [H|T])->
	max_item(MaxFun, T, H).

max_item(MaxFun, [H|T], Max)->
	max_item(MaxFun, T, MaxFun(Max,H));
max_item(_, [], Max)->
	Max.
