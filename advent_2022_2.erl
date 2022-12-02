-module(advent_2022_2).

-compile(export_all).

run(File, N, Strategy)
  when (is_integer(Strategy) andalso (Strategy == 1 orelse Strategy == 2)) andalso
    (is_integer(N) andalso (N == 1 orelse N == 2)) ->
    {ok, Fd} = file:open(File, [read]),
    case N of
        1 ->
            {ok, NthSumSorted} = total_calories(Fd, 0, [], 3),
            Result = lists:sum(NthSumSorted);
        2 ->
            Result = game(Fd, 0, Strategy)
    end,
    file:close(Fd),
    Result.
    
%% Advent Code 2022 #2
%%
-define(is_game_chars(C, M),
        (C == $A orelse C == $B orelse C == $C) andalso
        (M == $X orelse M == $Y orelse M == $Z)).

-define(is_sign_gt(C, M),
        (C == rock andalso M == scissors) orelse
        (C == paper andalso M == rock) orelse
        (C == scissors andalso M == paper)).
        
game(Fd, Sum, Strategy) ->
    case file:read_line(Fd) of
        {ok, [Competitor,_Space,Me|_]}
          when Strategy == 1 andalso ?is_game_chars(Competitor, Me) ->
            C = sign(Competitor),
            M = sign(Me),
            game(Fd, sum_round(C, M) + Sum, Strategy);
        
        {ok, [Competitor,_Space,Me|_]}
          when Strategy == 2 andalso ?is_game_chars(Competitor, Me) ->
            C0 = sign(Competitor),
            {C, M} = outcome_strategy(C0, outcome_by_char(Me)),
            game(Fd, sum_round(C, M) + Sum, Strategy);
        
        {ok, "\n"} ->
            game(Fd, Sum, Strategy);

        eof ->
            {ok, Sum}
    end.

outcome_strategy(SignC, Outcome) ->
    SignMe = lists:nth(outcome_clock_nth(Outcome), outcome_clock(SignC)),
    {SignC, SignMe}.

outcome_clock(SignC) -> outcome_clock(SignC, outcome_clock_init()).
    
outcome_clock_init() -> {[paper,scissors,rock,paper], rock}.

outcome_clock(SignC, {[SignC|_] = OutcomeClock, SignPrev}) ->
    [SignPrev|OutcomeClock];
outcome_clock(SignC, {[Sign|OutcomeClock], _SignPrev}) ->
    outcome_clock(SignC, {OutcomeClock, Sign}).

outcome_clock_nth(lose) -> 1;
outcome_clock_nth(draw) -> 2;
outcome_clock_nth(win)  -> 3.

outcome_by_char($X) -> lose;
outcome_by_char($Y) -> draw;
outcome_by_char($Z) -> win.

sign($A) -> rock;
sign($B) -> paper;
sign($C) -> scissors;
sign($X) -> rock;
sign($Y) -> paper;
sign($Z) -> scissors.

sum_round(C, M) -> point_my_sign(M) + point_round(C, M).

point_my_sign(rock)     -> 1;
point_my_sign(paper)    -> 2;
point_my_sign(scissors) -> 3.
    
point_round(Sign, Sign) -> 3;
point_round(SignC, SignM) when ?is_sign_gt(SignC, SignM) -> 0;
point_round(_SignC, _SignM) -> 6.

                                  



%% Advent Code 2022 #1
%%
total_calories(Fd, Sum, AllSumSorted, Nth) ->
    case file:read_line(Fd) of
        {ok, [D|_] = Data} when D >= $0 andalso D =< $9 ->
            Number = list_to_integer(lists:droplast(Data)),
            total_calories(Fd, Sum + Number, AllSumSorted, Nth);
        
        {ok, "\n"} ->
            NewAllSumSorted = 
                if length(AllSumSorted) >= Nth ->
                        tl(lists:merge([Sum], AllSumSorted));
                   true ->
                        lists:merge([Sum], AllSumSorted)
                end,
            total_calories(Fd, 0, NewAllSumSorted, Nth);

        eof ->
            {ok, AllSumSorted}
    end.
            
