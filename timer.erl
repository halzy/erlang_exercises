-module(timer).
-export([TC/4]).

TC = fun(TC_M, TC_F, TC_A, TC_N) when TC_N > 0 -> TC_L = tl([begin {TC_T, _Result} = timer:tc(TC_M, TC_F, TC_A), TC_T end || _ <- lists:seq(1, TC_N)]), TC_Min = lists:min(TC_L), TC_Max = lists:max(TC_L), TC_Med = lists:nth(round((TC_N - 1) / 2), lists:sort(TC_L)), TC_Avg = round(lists:foldl(fun(TC_X, TC_Sum) -> TC_X + TC_Sum end, 0, TC_L) / (TC_N - 1)), io:format("Range: ~b - ~b mics~nMedian: ~b mics ~nAverage: ~b mics~n", [TC_Min, TC_Max, TC_Med, TC_Avg]), TC_Med end.

