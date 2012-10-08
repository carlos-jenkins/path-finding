Path Finding
============

Some path finding algorithms written in Erlang:

- Best-First Greedy.
- A* .
- Jump points.

How to use
==========

- Compile the board module:
    ``1> c(board.erl).``
- Run start() from the board module:
    ``2> board:start().``
- Setup your environment in the visual editor.
- Compile the search module:
    ``3> c(search.erl).``
- Run the desired algorithm.
    ``4> search:greedy().``
    ``4> search:a_star().``
    ``4> search:jump_points().``
- Enjoy :)

