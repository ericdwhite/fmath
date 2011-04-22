-module(simplerandomwalk_tests).

-include_lib("eunit/include/eunit.hrl").

walk_test() ->
    RndNumbers = {1, [0.14, 0.56, 0.51, 0.34, 0.23, 0.78, 0.83, 0.92, 0.23, 0.34]},
    {RndFun, R} = fixed_random(RndNumbers),
    ?assertEqual(0.14, R),
    {_, R2} = RndFun(),
    ?assertEqual(0.57, R2).

fixed_random({Position, Values}) ->
    R = lists:nth(Position, Values),
    { fun() -> fixed_random({Position + 1, Values}) end, R}.

