Path Finding
============

Some path finding algorithms written in Erlang:

- Best-First Greedy.
- A* .
- Jump points.


Screenshots
===========

![Best-First Greedy in test map 1](/tests/test1_greedy.png "Best-First Greedy in test map 1")

![A* in test map 2](/tests/test2_a_star.png "A* in test map 2")

![Jump Points in test map 3](/tests/test3_jump_points.png "Jump Points in test map 3")


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


License
=======

Copyright (C) 2012 Carlos Jenkins <carlos@jenkins.co.cr>
Copyright (C) 2012 Pablo Rodriguez <pabloarb@gmail.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

