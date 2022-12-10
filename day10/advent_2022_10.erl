-module(advent_2022_10).

-compile(export_all).

%%
%% To note is that the whole screen in part 2 seemed
%% to be shifted 1 pixel to the left which
%% made me guess that the only possible first letter would be P.
%% The answer was PBZGRAZA.
%%
%% Introducint the threeshould 20, 60, 80 and so on, made
%% it a bit easier to realize when to calculate the strength signal.
%%
-define(rest(Val, D), (Val rem D)).

-define(crt_range(X, Cycle),
        (((X-1) =< ?rest(Cycle, 40)) andalso ((X+1) >= ?rest(Cycle,40)))).

run(File, Part) when Part == 1 orelse Part == 2 ->
    {ok, B} = file:read_file(File),
    L = [Y || X <- binary:split(B, <<"\n">>,  [global, trim_all]),
              Y <- [erlang:binary_to_list(X)]],
    Ans = case Part of
              1 -> 13440 = part_1(L); %% 13440;
              2 -> part_1(L) %% PBZGRAZA
          end,
    Ans.

%%
%% Part 1 and 2
%%
part_1(Ops) ->
    X = 1,
    Cycle = 0,
    Sum = 0,
    Threeshold = 20,
    signal_strength(Ops, X, Cycle, Threeshold, Sum).

signal_strength([Op|Ops], X, Cycle, Threeshold, Sum) ->
    XBeforeOp = X,
    {XNew, CycleNew} = op(Op, X, Cycle),
    S = strength(XBeforeOp, CycleNew, Threeshold),
    T = threeshold(S, Threeshold),
    signal_strength(Ops, XNew, CycleNew, T, Sum + S);
signal_strength([], _, _, _, Sum) ->
    Sum.

threeshold(S, Threeshold)
  when (S > 0) andalso (Threeshold == 20) ->
    Threeshold + 40;
threeshold(S, Threeshold) when (S > 0) ->
    Threeshold + 40;
threeshold(_S, Threeshold) ->
    Threeshold.

-define(n_touched(NTouched, PassedValue, NewValue),
        ((PassedValue < NTouched) andalso
         (NewValue >= NTouched))).

strength(X, Cycle, Threeshold) when Cycle >= Threeshold ->
    Threeshold * X;
strength(_X, _Cycle, _Threeshold) ->
    0.

op("noop" ++ _L, X, Cycle) ->
    sprite(X, Cycle),
    {X, Cycle + 1};
op("addx " ++ NumberStr, X, Cycle) ->
    N = n_from_str(NumberStr),
    sprite(X, Cycle),
    sprite(X, Cycle+1),
    {X + N, Cycle + 2}.

sprite(X, Cycle)
  when ((X > 0) andalso (X < 39)) andalso ?crt_range(X, Cycle) ->
    io:format("#"),
    newline(Cycle);
sprite(X, Cycle)
  when ((X == 0) andalso (?rest(Cycle, 40) =< 1)) ->
    io:format("#"),
    newline(Cycle);
sprite(X, Cycle)
  when ((X == 39) andalso (?rest(Cycle, 40) >= 38)) ->
    io:format("#"),
    newline(Cycle);
sprite(_X, Cycle) ->
    io:format("."),
    newline(Cycle).

newline(Cycle) when Cycle == 0 ->
    ok;
newline(Cycle) when Cycle > 0 andalso ?rest(Cycle, 40) == 39 ->
    io:format("~n", []);
newline(_) ->
    ok.
    
n_from_str(NumberStr) -> erlang:list_to_integer(NumberStr).
    


    
    


    
