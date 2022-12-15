-module(advent_2022_11).

-compile(export_all).

-record(m, {
           count = 0
          ,items = []
          ,op
          ,test_nr
          ,tr_ue
          ,fa_lse
         }).

run(File, Part) ->
    From = self(),
    _Pid = erlang:spawn(?MODULE, do, [File, Part, From]),
    receive
        {From, Ans} ->
            Ans
    end.

do(File, Part, From) ->
    {ok, Fd} = file:open(File, [read]),
    Ans = case Part of
              1 ->
                  316888 = big_monkey(Fd, maps:new(), 20, 3);
              2 ->
                  35270398814 = big_monkey(Fd, maps:new(), 10000, 1)
          end,
    file:close(Fd),
    From ! {From, Ans}.

%% print(Fmt, Args) -> io:format(Fmt, Args).
print(_Fmt, _Args) -> ok.
    

big_monkey(Fd, M, Rounds, WorryLevelDivider) ->
    {ok, M1} = bm(Fd, M),
    NMonkey = 1 + maps:get(monkey, M1),

    TestNrAll = [Y || #m{test_nr = Y} <- maps:values(M1)],
    Lcm = utils:lcm(TestNrAll),
    
    M2 = rounds(Rounds, NMonkey, M1, WorryLevelDivider, Lcm),
    Counters = [C ||
                   R = #m{} <- maps:values(M2),
                   C <- [R#m.count]],
    {[C1, C2], _} = lists:split(2, lists:reverse(lists:sort(Counters))),
    C1 * C2.

%%
%% Monkey inspects an item with a worry level of 79.
%%     Worry level is multiplied by 19 to 1501.
%%     Monkey gets bored with item. Worry level is divided by 3 to 500.
%%     Current worry level is not divisible by 23.
%%     Item with worry level 500 is thrown to monkey 3.
%%
rounds(0, _NMonkey, M, _WorryLevelDivider, _Lcm) ->
    M;
rounds(Round, NMonkey, M, WorryLevelDivider, Lcm) ->
    FirstMonkey = 0,
    M1 = one_round(FirstMonkey, NMonkey, M, WorryLevelDivider, Round, Lcm),
    rounds(Round-1, NMonkey, M1, WorryLevelDivider, Lcm).

one_round(Monkey, NMonkey, M, _WorryLevelDivider, _Round, _Lcm)
  when Monkey >= NMonkey ->
    M;
one_round(Monkey, NMonkey, M, WorryLevelDivider, Round, Lcm) ->
    Rec = maps:get(Monkey, M),
    M1 =
        case Rec#m.items of
            [] ->    M;
            Items -> do_items(Items, Monkey, Rec, M, WorryLevelDivider, Lcm)
        end,
    print("M:~p, Len:~p~n", [Monkey, length(Rec#m.items)]),
    one_round(Monkey+1, NMonkey, M1, WorryLevelDivider, Round, Lcm).

do_items([Item|Items], Monkey, Rec, M, WorryLevelDivider,Lcm) ->
    %% Rec = maps:get(Monkey, M),
    Op = Rec#m.op,
    New = Op(Item),
    WorryLevel =
        if WorryLevelDivider == 1 -> New rem Lcm;
           true -> New div WorryLevelDivider
        end,
    ThrowMonkey =
        case (WorryLevel rem Rec#m.test_nr) == 0 of
            true  ->
                print("M:~p, I:~p, WL:~p, B:~p, T:~p~n ",
                      [Monkey, Item, New, WorryLevel, Rec#m.tr_ue]),
                Rec#m.tr_ue;
            false ->
                print("M:~p, I:~p, WL:~p, B:~p, T:~p~n ",
                      [Monkey, Item, New, WorryLevel, Rec#m.fa_lse]),
                Rec#m.fa_lse
        end,
    M1 = throw_to(ThrowMonkey, WorryLevel, M),
    do_items(Items, Monkey, Rec#m{count = Rec#m.count + 1}, M1, WorryLevelDivider, Lcm);
do_items([], Monkey, Rec, M, _WorryLevelDivider, _Lcm) ->
    maps:put(Monkey, Rec#m{items = []}, M).


throw_to(MonkeyOther, Item, M) ->
    Rec = maps:get(MonkeyOther, M),
    Items = Rec#m.items ++ [Item],
    print("MO:~p, I:~p, RIs:~p, Is:~p~n", [MonkeyOther, Item, Rec#m.items, Items]),
    maps:put(MonkeyOther, Rec#m{items = Items}, M).

bm(Fd, M) ->
    Ith = maps:get(monkey, M, 0),
    Rec = maps:get(Ith, M, #m{}),

    case file:read_line(Fd) of
        {ok, "Monkey" ++ _Data} ->
            M1 = maps:put(Ith, Rec, M),
            bm(Fd, M1);
        
        {ok, "  Starting" ++ Data} ->
            IntegerAll = get_all_integers(Data),
            M1 = maps:put(Ith, Rec#m{items = IntegerAll}, M),
            bm(Fd, M1);

        {ok, "  Operation:" ++ Data} ->
            OpFun = get_op_fun(lists:droplast(Data)),
            M1 = maps:put(Ith, Rec#m{op = OpFun}, M),
            bm(Fd, M1);

        {ok, "  Test:" ++ Data} ->
            Int = get_integer(Data),
            M1 = maps:put(Ith, Rec#m{test_nr = Int}, M),
            bm(Fd, M1);

        {ok, "    If true:" ++ Data} ->
            Int = get_integer(Data),
            M1 = maps:put(Ith, Rec#m{tr_ue = Int}, M),
            bm(Fd, M1);

        {ok, "    If false:" ++ Data} ->
            Int = get_integer(Data),
            M1 = maps:put(Ith, Rec#m{fa_lse = Int}, M),
            bm(Fd, M1);

        {ok, "\n"} ->
            M1 = maps:put(monkey, Ith + 1, M),
            bm(Fd, M1);

        eof ->
            {ok, M}
    end.

get_op_fun(Data0) ->
    Data = string:trim(Data0),
    [_D , Operands] = string:split(Data, "="),
    case lists:member($+, Operands) of
        true ->
            [L, R] = string:split(Operands, "+"),
            plus_fun(string:trim(L), string:trim(R));
        false ->
            print("Data: ~p~n", [Operands]),
            [L, R] = string:split(Operands, "*"),
            mult_fun(string:trim(L), string:trim(R))
    end.

plus_fun("old"++_, "old"++_) -> fun(Current) -> Current + Current end;
plus_fun("old"++_, NumStr) ->   fun(Current) -> Current + get_integer(NumStr) end;
plus_fun(NumStr, "old"++_) ->   fun(Current) -> Current + get_integer(NumStr) end.

mult_fun("old", "old") ->  fun(Current) -> Current * Current end;
mult_fun("old", NumStr) -> fun(Current) -> Current * get_integer(NumStr) end;
mult_fun(NumStr, "old") -> fun(Current) -> Current * get_integer(NumStr) end.

            

get_all_integers(Data) ->
    case get_integer(Data, true) of
        {error, no_integer_found} ->
            [];

        {Int, DataRest} ->
            [Int | get_all_integers(DataRest)]

    end.

get_integer(Data) -> get_integer(Data, _WithRestData = false).

get_integer([Digit|_] = Data, WithRestData)
  when Digit >= $0 andalso Digit =< $9 ->
    case WithRestData of
        true  -> get_int(Data, []);
        false -> element(1,  get_int(Data, []))
    end;
get_integer([_|Data], WithRestData) ->
    get_integer(Data, WithRestData);
get_integer([], _WithRestData) ->
    {error, no_integer_found}.

get_int([Digit|Ds], IntStr)
  when Digit >= $0 andalso Digit =< $9 ->
    get_int(Ds, [Digit | IntStr]);
get_int(Data, IntStr) ->
    {list_to_integer(lists:reverse(IntStr)), Data}.
     

                  
    
