%% Copyright (C) 2008, Jose Castro
%% File    : othelloDisplay.erl
%% Author  : Jose Castro
%% Purpose : OthelloDisplay
%% Usage   : tetris:start().

-module(board).

-vsn(1).
-author('jose.r.castro@gmail.com').

-compile(export_all).
-export([start/0]).

-import(lists, [foreach/2, seq/2]).

-define(CELLSIZE, 20).
-define(CANVASSIZE, 600).
-define(ROWSIZE, ?CANVASSIZE div ?CELLSIZE).
-define(EMPTY,  0).
-define(WALL,   1).
-define(START,  2).
-define(FINISH, 3).
-define(FRINGE, 4).
-define(VISITED,5).
-define(CURRENT,6).
-define(NEIGHBORS_OFFSET, [{-1,-1}, {-1,0},{-1,1},{0,-1},{0,1},{1,-1},{1,0},{1,1}]).

board() ->
    N = ?ROWSIZE+2,
    list_to_tuple(lists:map(fun(_) -> ?EMPTY end, lists:seq(1, N*N))).

start() ->
    register(board,
	     spawn(fun() ->
			   GS     = gs:start(),
			   Win    = gs:create(window, GS, [{width, ?CANVASSIZE+100}, {height, ?CANVASSIZE+50}, {title, "Maze"}, {map, true}]),
			   Canvas = gs:create(canvas, canvas, Win, [{x,5},{y,5},{width,?CANVASSIZE+1},{height,?CANVASSIZE+1}, {buttonpress, true}]),
			   gs:create(button, exit,   Win, [{x,5},   {y,?CANVASSIZE+10}, {label, {text, "Exit"}}]),
			   gs:create(button, load,   Win, [{x,105}, {y,?CANVASSIZE+10}, {label, {text, "Load"}}]),
			   gs:create(button, save,   Win, [{x,205}, {y,?CANVASSIZE+10}, {label, {text, "Save"}}]),
			   gs:create(button, reset,  Win, [{x,305}, {y,?CANVASSIZE+10}, {label, {text, "Reset"}}]),
			   gs:create(entry,  text,   Win, [{x,505}, {y,?CANVASSIZE+10}, {text, ""}]),
			   gs:radiobutton(rb1, Win, [{label,{text,"obstacle"}}, {value, wall},   {y,  10}, {x,?CANVASSIZE+10}, {select, true}]),
			   gs:radiobutton(rb2, Win, [{label,{text,"start"}},    {value, start},  {y,  50}, {x,?CANVASSIZE+10}]),
			   gs:radiobutton(rb3, Win, [{label,{text,"finish"}},   {value, finish}, {y, 100}, {x,?CANVASSIZE+10}]),
			   drawLines(Canvas, 0, ?CELLSIZE, ?CANVASSIZE),
			   put(start,{-1,-1}),
			   put(finish,{-1,-1}),
			   TmpB  = setStart(0,0,board()),
			   Board = setFinish(?ROWSIZE-1, ?ROWSIZE-1, TmpB),
			   changeDisplay(Canvas, board(), Board),
			   eventLoop(Canvas, Board, get(start), obstacle)
		   end)).

eventLoop(Canvas, Board, Pos, SquareType) ->
    receive
	{gs, canvas, buttonpress, [], [1,X,Y|_]} ->
	    eventLoop(Canvas, movida(Canvas, X, Y, Board, SquareType), Pos, SquareType);
	{gs, rb1, click, _, _} ->
	    eventLoop(Canvas, Board, Pos, obstacle);
	{gs, rb2, click, _, _} ->
	    eventLoop(Canvas, Board, Pos, start);
	{gs, rb3, click, _, _} ->
	    eventLoop(Canvas, Board, Pos, finish);
	{gs, reset, click, _, _} ->
	    TmpB  = setStart(0,0,board()),
	    NewBoard = setFinish(?ROWSIZE-1, ?ROWSIZE-1, TmpB),
	    changeDisplay(Canvas, Board, NewBoard),
	    eventLoop(Canvas, NewBoard, get(start), obstacle);

	{gs, load, click, _, _} ->
	    FileName = gs:read(text, text),
	    case filelib:is_file(FileName) of
		false -> eventLoop(Canvas, Board, Pos, SquareType);
		true  ->
		    NewBoard = loadFile(FileName),
		    changeDisplay(Canvas, Board, NewBoard),
		    eventLoop(Canvas, NewBoard, Pos, SquareType)
	    end;

	{get_pos, Proc} ->
	    Proc ! Pos,
	    eventLoop(Canvas, Board, Pos, SquareType);

	{get_finish, Proc} ->
        Finish = get(finish),
	    Proc ! Finish,
	    eventLoop(Canvas, Board, Pos, SquareType);

	{get_neighbors, Proc} ->
	    Neighbors = neighbors(Board, Pos),
	    io:format("neighbors ~w~n", [Neighbors]),
	    Proc ! {Neighbors, lists:map(fun (X) -> h(X) end, Neighbors)},
	    NewBoard = updateCells(Board, Neighbors, ?FRINGE),
	    changeDisplay(Canvas, Board, NewBoard),
	    eventLoop(Canvas, NewBoard, Pos, SquareType);

	{move, NewPos = {Row,Col}} ->
	    Cell = calcPos(Row, Col),
	    case element(Cell, Board) of
		?FRINGE ->
		    NewBoard = updateCells(updateCells(Board, [Pos], ?VISITED), [NewPos], ?CURRENT),
		    changeDisplay(Canvas, Board, NewBoard),
		    eventLoop(Canvas, NewBoard, NewPos, SquareType);
		_ ->
		    eventLoop(Canvas, Board, Pos, SquareType)
	    end;
	{gs, save, click, _, _} ->
	    FileName = gs:read(text,text),
	    Start  = get(start),
	    Finish = get(finish),
	    unconsult(FileName, [Start,Finish,Board]),
	    eventLoop(Canvas, Board, Pos, SquareType);

	{gs, _, destroy, _, _} ->
	    gs:stop();
	{gs, exit, click, _, _} ->
	    gs:stop();
	X ->
	    io:format("received ~w~n", [X]),
	    eventLoop(Canvas, Board, Pos, SquareType)
    end.

neighbors(Board, Pos) ->
    neighbors(Board, Pos, ?NEIGHBORS_OFFSET).

h({Row, Col}) ->
    {R,C} = get(finish),
    math:sqrt((Row-R)*(Row-R)+(Col-C)*(Col-C)).

neighbors(_,_,[]) -> [];
neighbors(Board,Cell={R,C},[{OR,OC}|Rest]) ->
    Pos = calcPos(R+OR,C+OC),
    if
	Pos == invalid -> neighbors(Board, Cell, Rest);
	true ->
	    case element(Pos, Board) of
		?EMPTY ->
		    [{R+OR,C+OC}|neighbors(Board, Cell, Rest)];
		_ ->
		    neighbors(Board, Cell, Rest)
	    end
    end.

updateCells(Board, [], _) -> Board;
updateCells(Board, [{R,C}|Rest], Color) ->
    Pos = calcPos(R,C),
    updateCells(setelement(Pos, Board, Color), Rest, Color).

unconsult(File, L) ->
    {ok, S} = file:open(File, write),
    lists:foreach(fun(X) -> io:format(S, "~p.~n", [X]) end, L),
    file:close(S).

loadFile(FileName) ->
    {ok, [Start, Finish, NewBoard]} = file:consult(FileName),
    put(start, Start),
    put(finish, Finish),
    NewBoard.

calcPos(Row, _) when Row < 0 -> invalid;
calcPos(Row, _) when Row >= ?ROWSIZE -> invalid;
calcPos(_,Col ) when Col < 0 -> invalid;
calcPos(_,Col ) when Col >= ?ROWSIZE -> invalid;
calcPos(Row, Col) ->
    N = ?ROWSIZE + 2,
    (Row+1)*N + Col + 2.

setStart(Row, Col, Board) ->
    {R, C} = get(start),
    put(start, {Row, Col}),
    W = calcPos(Row, Col),
    if
	R >= 0 ->
	    Pos = calcPos(R,C),
	    B = setelement(Pos, Board, ?EMPTY),
	    setelement(W, B, ?START);
	true ->
	    setelement(W, Board, ?START)
    end.

setFinish(Row, Col, Board) ->
    {R, C} = get(finish),
    put(finish, {Row, Col}),
    W = calcPos(Row, Col),
    if
	R >= 0 ->
	    Pos = calcPos(R,C),
	    B = setelement(Pos, Board, ?EMPTY),
	    setelement(W, B, ?FINISH);
	true ->
	    setelement(W, Board, ?FINISH)
    end.

movida(Canvas, X,Y, Board, SquareType) ->
    Col = X div ?CELLSIZE,
    Row = Y div ?CELLSIZE,
    case SquareType of
	obstacle ->
	    W = calcPos(Row, Col),
	    NewBoard = makeMove(W, Board),
	    changeDisplay(Canvas, Board, NewBoard);
	start ->
	    NewBoard = setStart(Row, Col, Board),
	    changeDisplay(Canvas, Board, NewBoard);
	finish ->
	    NewBoard = setFinish(Row, Col, Board),
	    changeDisplay(Canvas, Board, NewBoard)
    end.

makeMove(Pos, Board) ->
    case element(Pos, Board) of
	0 -> setelement(Pos, Board, 1);
	1 -> setelement(Pos, Board, 0);
	_ -> Board
    end.

changeDisplay(Canvas, Board, NewBoard) ->
    N = (?CANVASSIZE div ?CELLSIZE),
    RowSize = ?ROWSIZE + 2,
    foreach(fun(Y) ->
		    foreach(fun(X) ->
				    Pos = Y*RowSize + X + 1,
				    changeDisplay(Canvas, X,Y, element(Pos, Board), element(Pos, NewBoard))
			    end, seq(1,N))
	    end, seq(1,N)),
    NewBoard.

color(?WALL)    -> black;
color(?START)   -> green;
color(?FINISH)  -> red;
color(?FRINGE)  -> yellow;
color(?VISITED) -> blue;
color(?CURRENT) -> cyan.

changeDisplay(_, _, _, X, X) -> true;
changeDisplay(Canvas, X, Y, ?EMPTY, Value)  -> drawCircle(Canvas, X, Y, color(Value));
changeDisplay(     _, X, Y, _Value, ?EMPTY) -> gs:destroy(get({X,Y}));
changeDisplay(Canvas, X, Y, _Value, Value)  ->
    gs:destroy(get({X,Y})),
    drawCircle(Canvas, X, Y, color(Value)).

drawLines(_, Pos1, _, Pos2) when Pos1 > Pos2 -> true;
drawLines(Canvas, Pos, Increment, Last) ->
    gs:create(line, Canvas, [{coords, [{0,Pos},{?CANVASSIZE,Pos}]}, {width, 1}]),
    gs:create(line, Canvas, [{coords, [{Pos,0},{Pos,?CANVASSIZE}]}, {width, 1}]),
    drawLines(Canvas, Pos+Increment, Increment, Last).

drawCircle(Canvas, X,Y, Color) ->
    XX = (X-1)*?CELLSIZE + 2,
    YY = (Y-1)*?CELLSIZE + 2,
    Size = ?CELLSIZE - 4,
    put({X,Y}, gs:create(rectangle, Canvas, [{coords, [{XX,YY}, {XX+Size,YY+Size}]}, {fill, Color}])).
