-module(search).
-compile(export_all).

-import(board, []).

-define(WAIT, 500).
-define(WALL,   1).

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

is_walkable(Board, X, Y) ->
    Pos = board:calcPos(X, Y),
    if
    Pos == invalid -> false;
    true ->
        case element(Pos, Board) of
        ?WALL ->
            false;
        _ ->
            true
        end
    end.

identify_successors(Node) ->
    %%Neighbors = find_neighbors(Node),
    implement.

find_neighbors(Node) ->
    implement.

jump({X, Y, Px, Py}, Payload = {Board, {Ex, Ey}}) ->

    Dx = X - Px,
    Dy = Y - Py,

    %% Simple cases
    Wall = not is_walkable(Board, X, Y),
    if
    Wall ->
        {};
    (X == Ex) and (Ey == Ey) ->
        {X, Y};

    %% Check for forced neighbors
    true ->
        %% Moving along the diagonal
        Diagonal = ((Dx /= 0) and (Dy /= 0)) and ((
                        is_walkable(Board, X - Dx, Y + Dy) and not
                        is_walkable(Board, X - Dx, Y)
                    ) or (
                        is_walkable(Board, X + Dx, Y - Dy) and not
                        is_walkable(Board, X, Y - Dy)
                    )),
        %% Moving horizontally / vertically
        %%  Moving along X axis
        AlongX = Diagonal or ((Dx /= 0) and ((
            is_walkable(Board, X + Dx, Y + 1) and not
            is_walkable(Board, X, Y + 1)
        ) or (
            is_walkable(Board, X + Dx, Y - 1) and not
            is_walkable(Board, X, Y - 1)
        ))),
        %%  Moving along Y axis
        AlongY = AlongX or ((
            is_walkable(Board, X + 1, Y + Dy) and not
            is_walkable(Board, X + 1, Y)
        ) or (
            is_walkable(Board, X - 1, Y + Dy) and not
            is_walkable(Board, X - 1, Y)
        )),
        if
        Diagonal; AlongX; AlongY -> {X, Y};
        true ->
            %% Special cases with recursive jumps
            %%  When moving diagonally, must check for vertical / horizontal
            %%  jump points
            if
            (Dx /= 0) and (Dy /= 0) ->
                Jx = jump({X + Dx, Y, X, Y}, Payload),
                Jy = jump({X, Y + Dy, X, Y}, Payload),
                if
                (Jx /= {}) or (Jy /= {}) -> {X, Y};
                true ->
            %%  Moving diagonally, must make sure one of the vertical/horizontal
            %%  neighbors is open to allow the path
                    AllowPath = is_walkable(Board, X + Dx, Y) or
                                is_walkable(Board, X, Y + Dy),
                    if
                        AllowPath -> jump({X + Dx, Y + Dy, X, Y}, Payload);
                        true -> {}
                    end
                end
            end
        end
    end.
