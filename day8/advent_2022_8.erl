-module(advent_2022_8).

-compile(export_all).

-define(is_digit(D), ((D >= $0) andalso (D =< $9))).

%%
%% A solution using lists storing only the numbers of the tree hight
%% became too difficult to handle due to some cases were
%% there are overlapping viewings of a tree.
%%
%% When the stored entity were changed to
%%   {TreeHight, <1 for seen, 0 for not seen>}
%% part 1 was solvable.
%%
%% Part 2 however became too difficult as here you had to be able to traverse
%% the two X and Y directions for the same considered midpoint-tree.
%% To solve part 2 I hade to start using a maps with the key {X, Y}.
%%

run(File, Part) when Part == 1 orelse Part == 2 ->
    {ok, Fd} = file:open(File, [read]),
    Ans = case Part of
              1 ->
                  1647 == sum_seen_trees(Fd),
                  1647;
              2 ->
                  392080 == scenic_score(Fd),
                  392080.
          end,
    file:close(Fd),
    Ans.

%%
%% Part 2 
%%
scenic_score(Fd) ->
    {ok, Map} = get_grid_from_file(Fd, maps:new()),
    {XMax, YMax} = maps:get(size, Map),
    MaxScore = 0,
    calc_score(1, 1, {XMax, YMax}, MaxScore, Map). 

print(Fmt, Args, true)    -> io:format(Fmt, Args);
print(_Fmt, _Args, false) -> ok.

x_score(X, Y, Step, Min, Max, Value, Map, Sum)
  when (X >= Min) andalso (X =< Max) ->
    TreeVal = maps:get({X,Y}, Map),
    if
        Value > TreeVal ->
            x_score(X+Step, Y, Step, Min, Max, Value, Map, Sum+1);
        Value =< TreeVal ->
            Sum + 1
    end;
x_score(_X, _Y, _Step, _Min, _Max, _Value, _Map, Sum) ->
    Sum.
    
y_score(X, Y, Step, Min, Max, Value, Map, Sum)
  when (Y >= Min) andalso (Y =< Max) ->
    TreeVal = maps:get({X,Y}, Map),
    if
        Value > TreeVal ->
            y_score(X, Y+Step, Step, Min, Max, Value, Map, Sum+1);
        Value =< TreeVal ->
            Sum + 1
    end;
y_score(_X, _Y, _Step, _Min, _Max, _Value, _Map, Sum) ->
    Sum.

calc_score(_X, Y, {_XMax, YMax}, MaxScore, _Map) when (Y > YMax) ->
    MaxScore;
calc_score(X, Y, {XMax, YMax}, MaxScore, Map) when (X > XMax) ->
    calc_score(1, Y+1, {XMax, YMax}, MaxScore, Map);
calc_score(X, Y, {XMax, YMax}, MaxScore, Map) ->
    TreeVal = maps:get({X,Y}, Map),
    L = x_score(X-1, Y, -1, 1, XMax, TreeVal, Map, 0),
    R = x_score(X+1, Y,  1, 1, XMax, TreeVal, Map, 0),
    U = y_score(X, Y+1,  1, 1, XMax, TreeVal, Map, 0),
    D = y_score(X, Y-1, -1, 1, XMax, TreeVal, Map, 0),
    Prod = L * R * U * D,
    MaxScoreNew = if Prod > MaxScore -> Prod; true -> MaxScore end,
    calc_score(X+1, Y, {XMax, YMax}, MaxScoreNew, Map).
    

get_grid_from_file(Fd, Map) -> get_grid_from_file(Fd, 1, Map).

get_grid_from_file(Fd, Y, Map) ->
    case file:read_line(Fd) of
        {ok, ARow} ->
            Row = lists:droplast(ARow),
            PutCoord = fun(E, {X, M}) -> {X+1, maps:put({X, Y}, E - $0, M)} end,
            {_, MapNew} = lists:foldl(PutCoord, {1, Map}, Row),
            get_grid_from_file(Fd, Y+1, maps:put(size, {length(Row), Y}, MapNew));

        eof ->
            {ok, Map}
    end.




%%
%% Part 1 solved by using lists.
%%
-define(hight(Entity), element(1, Entity)).
-define(seen(Entity),  erlang:setelement(2, Entity, 1)).

sum_seen_trees(Fd) ->
    Row = [],
    {ok, Rs} = get_from_file(Fd, Row),
    Rs,
    Nth = 1,
    LinesRev = [],
    Rows = sum_visible(Rs, Nth, LinesRev),
    Cs = add_to_columns(Rows, []),
    Cols = sum_visible(Cs, Nth, LinesRev),
    lists:foldl(fun({_Hight, Seen}, Sum) -> Sum + Seen end,
                0,
                lists:flatten(Cols)).
    
get_from_file(Fd, Rs) ->
    case file:read_line(Fd) of
        {ok, ARow} ->
            Row = lists:droplast(ARow),
            io:format("ROW:~s~n", [Row]),
            RowNew = row_convert(Row),
            get_from_file(Fd, [RowNew|Rs]);

        eof ->
            {ok, lists:reverse(Rs)}
    end.

sum_visible([Line|Lines], Nth, LinesRev) ->
    Line1 = sum_line(Line, -1, []),
    Line2 = lists:reverse(sum_line(lists:reverse(Line1), -1, [])),
    sum_visible(Lines, Nth+1, [Line2|LinesRev]);
sum_visible([], _Nth, LinesRev) ->
    lists:reverse(LinesRev).

sum_line([L|Ls], Max, LsRev) ->
    if ?hight(L) > Max ->
            sum_line(Ls, ?hight(L), [?seen(L)|LsRev]);
       true ->
            sum_line(Ls, Max, [L|LsRev])
    end;
sum_line([], _Max, LsRev) ->
    lists:reverse(LsRev).

row_convert([Digit|Line]) when ?is_digit(Digit) ->
    [{Digit - $0, 0} | row_convert(Line)];
row_convert(_) ->
    [].

update_to_tuple([C|Cs]) -> [update_to_tuple_2(C)|update_to_tuple(Cs)];
update_to_tuple([]) -> [].

update_to_tuple_2([E|Line]) -> [{E,0}|update_to_tuple_2(Line)];
update_to_tuple_2([]) -> [].

add_to_columns(Row, []) ->
    Cs = lists:map(fun(_E) -> [] end, hd(Row)),
    add_to_columns_1(Row, Cs).

add_to_columns_1([Row], Cs) ->
    add_to_columns_2(Row, Cs, rev);
add_to_columns_1([Row|Rs], Cs) ->
    CsNew = add_to_columns_2(Row, Cs, no_rev),
    add_to_columns_1(Rs, CsNew);
add_to_columns_1([], []) ->
    [].

add_to_columns_2([C|Row], [Col|Cs], no_rev) ->
    [[C|Col] | add_to_columns_2(Row, Cs, no_rev)];
add_to_columns_2([C|Row], [Col|Cs], rev) ->
    [lists:reverse([C|Col]) | add_to_columns_2(Row, Cs, rev)];
add_to_columns_2([], _, _) ->
    [].

    



