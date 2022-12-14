-module(advent_2022_12).

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
                  elev_steps(Fd);
              2 ->
                  false
          end,
    file:close(Fd),
    Ans.


elev_steps(Fd) ->
    {ok, M} = get_grid_from_file(Fd, maps:new()),
    walk(maps:get(start, M), maps:get(goal, M), M).


walk(StartPos, GoalPos, M0) ->
    M = maps:put(StartPos, 0, M0),
    print("Start:~p:~p, End:~p:~p, Max:~p~n",
          [StartPos, maps:get(StartPos, M), GoalPos, maps:get(GoalPos, M), maps:get(size, M)]),
    Steps = 0,
    GoBack = [StartPos],
    BeenThere = gb_sets:add(StartPos, gb_sets:new()),
    Dirs = get_directions(StartPos, BeenThere, [], M),
    Elevation = maps:get(StartPos, M),
    walk({Dirs, Elevation, StartPos, GoalPos, Steps, GoBack, BeenThere, M}).

walk({[Dir|Ds], Eval, Pos, GoalPos, Steps, GoBack, BeenThere, M}) ->
    E = maps:get(Dir, M),
    case Dir of
        GoalPos ->
            print(">>>>> ~p = GoalPos = Dir = ~p~n", [GoalPos, Dir]),
            print(">>>>> Eval = ~p ~n", [E]),
            print(">>>>> len(BeenThere) = ~p~n", [gb_sets:size(BeenThere)]),
            print(">>>>> len(GoBack) = ~p~n", [length(GoBack)]),
            Steps + 1;
        
        _Pos when (Eval + 1) >= E ->
            try
                %% Step to Dir
                %% Crossroad =
                %%    fun() -> walk(Ds, Eval, Pos, GoalPos, Steps, GoBack, BeenThere, M) end,
                %% Crossroad = {Ds, Eval, Pos, GoalPos, Steps, GoBack, BeenThere, M},
                DirsNext = get_directions(Dir, BeenThere, [Pos], M),
                GoBackNext = [Dir|GoBack],
                BeenThereNew = gb_sets:add(Dir, BeenThere),
                print("1:P:~p, E:~p, LGB:~p~n", [Dir, E, length(GoBackNext)]),
                walk({DirsNext, E, Dir, GoalPos, Steps+1, GoBackNext, BeenThereNew, M})
            catch
                throw:empty_dirs ->
                    case Ds of
                        [] ->
                            print("empty_dirs~n", []),
                            throw({been_there, gb_sets:add(Dir, BeenThere)});
                        _Ds ->
                            print("empty_dirs:~p~n", [length(Ds)]),
                            walk({Ds, Eval, Pos, GoalPos, Steps, GoBack, BeenThere, M})
                    end;

                throw:{been_there, BT} ->
                    case Ds of
                        [] ->
                            print("been_there, BT:~p~n", [gb_sets:size(BT)]),
                            throw({been_there, gb_sets:add(Dir, BT)});
                        _Ds ->
                            print("been_there:~p, BT:~p~n",
                                      [length(Ds), gb_sets:size(BT)]),
                            walk({Ds, Eval, Pos, GoalPos, Steps, GoBack, BT, M})
                    end
            end;

        _Pos ->
            walk({Ds, Eval, Pos, GoalPos, Steps, GoBack, BeenThere, M})
    end;
walk({[], _Eval, Pos, _GoalPos, _Steps, _GoBack, _BeenThere, _M}) ->
    print("2:P:~p, E:~p, LGB:~p~n", [Pos, _Eval, length(_GoBack)]),
    throw(empty_dirs).
    %%    GB = element(6, WalkTuple),
    %%    {Ds, E, P, GP, S, G, M} = setelement(6, WalkTuple, GB ++ [Pos]),
    %%    walk({Ds, E, P, GP, S, G, M}).
    %% 
    %% Pos leads no where and that must be regardless of the Eval level.
    %% Thus add Pos at the end of GoBack to become practially unreachable.
    %%
    %% print_list(GoBack),
    %% print("2:GB:~p~n~n", [GoBack]),
    %% IAmBack = lists:dropwhile(fun(Fun) when is_function(Fun) -> false;
    %%                             (_BackPos) -> true  end,
    %%                         GoBack),
    %%IAmBack = lists:dropwhile(fun({_,_}) -> true;
    %%                             (_WalkTuple) -> false  end,
    %%                         GoBack),
    %% [WalkTuple|_] = IAmBack,
    %% Crossroad().
    

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

get_grid_from_file(Fd, Map) -> get_grid_from_file(Fd, 1, Map).

get_grid_from_file(Fd, Y, Map) ->
    case file:read_line(Fd) of
        {ok, ARow} ->
            Row = lists:droplast(ARow),
            PutCoord =
                fun(E, {X, M}) ->
                        case E of
                            %% S = 0, $a = 1, $z = 26, E = 27
                            $S ->
                                M1 = maps:put({X, Y}, 0, M),
                                {X+1, maps:put(start, {X,Y}, M1)};
                            $E ->
                                M1 = maps:put({X, Y}, $z - $a + 2, M),
                                {X+1, maps:put(goal, {X,Y}, M1)};
                            _  ->
                                ElevateValue = E - $a + 1,
                                M1 = maps:put({X, Y}, ElevateValue, M),
                                {X+1, M1}
                        end
                end,
            {_, MapNew} = lists:foldl(PutCoord, {1, Map}, Row),
            get_grid_from_file(Fd, Y+1, maps:put(size, {length(Row), Y}, MapNew));

        eof ->
            {ok, Map}
    end.

