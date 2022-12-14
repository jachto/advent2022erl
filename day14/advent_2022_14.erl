-module(advent_2022_14).

-compile(export_all).

%%-record(m, {
%%         }).

%% "abcdefghijklmnopqrstuvwxyz".

print(Fmt, Args) -> io:format(Fmt, Args).
%% print(Fmt, Args) -> ok.
    

run(File, Part) ->
    {ok, Fd} = file:open(File, [read]),
    Ans = case Part of
              1 ->
                  do_sand(Fd);
              2 ->
                  false
          end,
    file:close(Fd),
    Ans.


do_sand(Fd) ->
    {ok, M} = get_grid_from_file(Fd, maps:new()),
    print_rock({457, 120}, {490, 160}, init, M).




print_list([F|GoB]) when is_function(F) ->
    print("F, ", []),
    print_list(GoB);
print_list([P|GoB]) ->
    print("P:~p, ", [P]),
    print_list(GoB);
print_list([]) ->
    exit(my_ok).

is_in_rangs({X, Y}, M) ->
    {MaxX, MaxY} = maps:get(size, M),
    if X > MaxX orelse Y > MaxY orelse X < 1 orelse Y < 1 ->
            false;
       true ->
            true
    end.

add_pos({X, Y}, {U, V}) -> {X+U, Y+V}.
            
get_directions(Pos, BeenThere, _CameFrom, M) ->
    Dirs = [NewPos ||
               Dir <- all_directions(),
               NewPos <- [add_pos(Dir, Pos)],
               %% not(lists:member(NewPos, CameFrom)), %% Probably superfluous:GoBack!
               is_in_range(NewPos, M),
               not(gb_sets:is_member(NewPos, BeenThere))
           ],
    ElevPred = fun(Pos1, Pos2) -> maps:get(Pos1, M) >= maps:get(Pos2, M) end,
    D = lists:sort(ElevPred, Dirs),
    print("P:~p, D:~p, CF:~p~n", [Pos, D, _CameFrom]),
    D.

is_in_range({X, Y}, M) ->
    {MaxX, MaxY} = maps:get(size, M),
    if X > MaxX orelse Y > MaxY orelse
       X < 1    orelse Y < 1 -> 
            false;
       true ->
            true
    end.

add_to_go_back([], Pos, _Crossroad, GoBack) -> [Pos|GoBack];
add_to_go_back(_Ds, Pos, Crossroad, GoBack)  -> [Pos,Crossroad|GoBack].

all_directions() -> [{1,0},{0,1},{-1,0},{0,-1}].

get_grid_from_file(Fd, Map) ->
    get_grid_from_file2(Fd, maps:put(size, {0,0}, Map)).

get_grid_from_file2(Fd, M) ->
    case file:read_line(Fd) of
        {ok, ARow} ->
            Row = lists:droplast(ARow),
            M1 = scan_line(Row, M),
            get_grid_from_file2(Fd, M1);

        eof ->
            {ok, M}
    end.

scan_line(Line, M) ->
    [Pair|C] = string:split(Line, " -> "),
    [Pos] = make_pos([Pair]),
    scan_coord(Pos, C, M).

scan_coord(Pos1, [_|_] = Coords, M) ->
    [Pair2|Cs] = string:split(Coords, " -> "),
    [Pos2] = make_pos([Pair2]),
    M1 = new_max(Pos1, maps:get(size, M), M),
    M2 = scan(Pos1, Pos2, M1),    
    scan_coord(Pos2, Cs, M2);
scan_coord(_Pos, [], M) ->
    M.

scan({X, Y1}, {X, Y2}, M) ->
    Ys = lists:seq(Y1, Y2, (Y2-Y1) div abs(Y2-Y1)),
    add_rock(X, Ys, M);
scan({X1, Y}, {X2, Y}, M) ->
    Xs = lists:seq(X1, X2, (X2-X1) div abs(X2-X1)),
    add_rock(Xs, Y, M).

make_pos([Pair|Ps]) ->
    [S1,S2] = string:split(Pair, ","),
    Pos = {list_to_integer(S1), list_to_integer(S2)},
    [Pos|make_pos(Ps)];
make_pos([]) ->
    [].

add_rock([X|Xs], Y, M) -> add_rock(Xs, Y, maps:put({X,Y}, rock, M));
add_rock(X, [Y|Ys], M) -> add_rock(X, Ys, maps:put({X,Y}, rock, M));
add_rock(_Xs, _Ys,  M) -> M.

new_max({X1, Y1}, {MaxX, MaxY}, M) ->
    maps:put(size, {max(X1, MaxX), max(Y1, MaxY)}, M).
    
print_rock({X1, Y1}, {X2, Y2}, init, M) when X2 > X1 andalso Y2 > Y1 ->
    Xs = lists:seq(X1, X2, 1),
    Ys = lists:seq(Y1, Y2, 1),
    print_rock2(Xs, Ys, Xs, M).

print_rock2([X|Xs], [Y|_] = Ys, AllX, M) ->
    case maps:get({X,Y}, M, air) of
        rock -> io:format("#", []);
        air  -> io:format(".", [])
    end,
    print_rock2(Xs, Ys, AllX, M);
print_rock2(_Xs, [], _AllX, _M)   -> ok;
print_rock2([], [_Y|Ys], AllX, M) ->
    io:format("~n", []),
    print_rock2(AllX, Ys, AllX, M).


    

