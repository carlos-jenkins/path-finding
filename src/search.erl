-module(search).
-compile(export_all).

-import(board, []).

-define(WAIT, 500).

%%%%%%%%%%%%%%%%%%%%%
%% HELPERS
%%%%%%%%%%%%%%%%%%%%%

%% Wait function
wait(Mill) ->
    receive
    after Mill -> ok
    end.

%% Index of the minimum
minimum([]) -> error;
minimum([Min]) -> {Min, 1};
minimum([Head|Tail]) -> minimum(Head, Tail, 1, 2).
minimum(Min, [Head|Tail], Index, CIndex) ->
    if
        Head < Min ->
            minimum(Head, Tail, CIndex, CIndex + 1);
        true ->
            minimum(Min, Tail, Index, CIndex + 1)
    end;
minimum(Min, [], Index, _) -> {Min, Index}.


%%%%%%%%%%%%%%%%%%%%%
%% GREEDY
%%%%%%%%%%%%%%%%%%%%%

%% Greedy process
greedy_proc({Fi, Fj}, {FringeCell, FringeHeu}) ->

    %% Do wait
    wait(?WAIT),

    %% Check if found
    board ! {get_pos, self()},
    receive
        {I, J} when (I == Fi) and (J == Fj) ->
            found;
        {_, _} ->
            %% Jump
            board ! {get_neighbors, self()},
            receive
                {[], _} ->
                    if FringeCell == [] ->
                        io:format("Unable to find the path.~n"),
                        fail;
                    true ->
                        io:format("Problems, got stuck.~n"),
                        %% Move
                        {Val, Min} = minimum(FringeHeu),
                        Next = lists:nth(Min, FringeCell),
                        io:format("Will try to backtrack to ~w.~n", [Next]),
                        board ! {move, Next},
                        %% Remove new search point from Fringe
                        greedy_proc({Fi, Fj},
                                    {lists:delete(Next, FringeCell),
                                     lists:delete(Val, FringeHeu)})
                    end;
                {Cells, Heuristics} ->
                    %% Move
                    {Val, Min} = minimum(Heuristics),
                    Next = lists:nth(Min, Cells),
                    board ! {move, Next},
                    %% Add to Fringe
                    greedy_proc({Fi, Fj},
                                {lists:append(FringeCell,
                                                lists:delete(Next, Cells)),
                                 lists:append(FringeHeu,
                                                lists:delete(Val, Heuristics))})
            end
    end.

%% Spawn greedy
greedy() ->
    board ! {get_finish, self()},
    receive
        {Fi, Fj} ->
            spawn(search, greedy_proc, [{Fi, Fj}, {[], []}])
    end.


%%%%%%%%%%%%%%%%%%%%%
%% A*
%%%%%%%%%%%%%%%%%%%%%

%% A* process
a_star_proc({Si, Sj}, {Fi, Fj}, {FringeCell, FringeHeu}) ->

    %% Do wait
    wait(?WAIT),

    %% Check if found
    board ! {get_pos, self()},
    receive
        {I, J} when (I == Fi) and (J == Fj) ->
            found;
        {_, _} ->
            %% Jump
            board ! {get_neighbors, self()},
            receive
                {[], _} ->
                    if FringeCell == [] ->
                        io:format("Unable to find the path.~n"),
                        fail;
                    true ->
                        io:format("Problems, got stuck.~n"),
                        %% Move
                        {Val, Min} = minimum(FringeHeu),
                        Next = lists:nth(Min, FringeCell),
                        io:format("Will try to backtrack to ~w.~n", [Next]),
                        board ! {move, Next},
                        %% Remove new search point from Fringe
                        a_star_proc({Si, Sj},
                                    {Fi, Fj},
                                    {lists:delete(Next, FringeCell),
                                     lists:delete(Val, FringeHeu)})
                    end;
                {Cells, Heuristics} ->
                    AStarHeuristics = a_star_logic(Heuristics, Cells, {Si, Sj}),
                    %% Add to Fringe
                    NewFringeCell = lists:append(FringeCell, Cells),
                    NewFringeHeu =  lists:append(FringeHeu, AStarHeuristics),
                    %% Move
                    {Val, Min} = minimum(NewFringeHeu),
                    Next = lists:nth(Min, NewFringeCell),
                    board ! {move, Next},
                    a_star_proc({Si, Sj},
                                {Fi, Fj},
                                {lists:delete(Next, NewFringeCell),
                                 lists:delete(Val, NewFringeHeu)})
            end
    end.

%% A* calc for distance traveled plus heuristics.
a_star_logic([F|R], [{Pi, Pj}|RC], {Si, Sj}) ->
    [
        (F + math:sqrt((Pi - Si) * (Pi - Si) + (Pj - Sj) * (Pj - Sj))) |
        a_star_logic(R, RC, {Si, Sj})
    ];
a_star_logic([], _, _) -> [].

%% Spawn A*
a_star() ->
    board ! {get_finish, self()},
    receive
        {Fi, Fj} ->
        board ! {get_pos, self()},
        receive
            {Si, Sj} ->
                spawn(search, a_star_proc, [{Si, Sj}, {Fi, Fj}, {[], []}])
        end
    end.


%%%%%%%%%%%%%%%%%%%%%
%% Jump points
%%%%%%%%%%%%%%%%%%%%%
