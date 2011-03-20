-module(simplerandomwalk).
-export([walk/1]).

walk(N) ->
    A = [], 
    walk(N, A).

walk(0, L) ->
    L;
walk(N, L) ->
    R = random:uniform(),
    % io:format("walk ~w~n", [R]),
    if 
        R > 0.5 -> 
            L1 = lists:append(L, [true]);
        true -> % else
            L1 = lists:append(L, [false])
    end,
    % io:format("result: ~w~n", [L1]),
    walk(N-1, L1).
