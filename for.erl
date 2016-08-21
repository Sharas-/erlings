-module(for).
-export([for/3]).

for(_, N, N) -> [];
for(F, I, N) ->
	[F(I) | for(F, I+1, N)].
	

