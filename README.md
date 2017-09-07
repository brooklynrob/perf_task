# Performance Task Overview
## Overview
* A turtle exists on an x, y graph (a Cartesian coodinate graph)
* The turtle starts at point 0, 0 facing north
* The turtle is fed a series of commands from an input file that will move the turtle around the graph
* The turtle must stop its move is its next move would move into an obstacle - obstacles exist throughout the graph (limited to 10 total obstacles)
* THE GOAL of the game/program is find the maximum eucilidian distance that the turtle is away from 0,0 during the course of its moves

## Input file
* The lines of the input file are either one or two chars separated by a space
	* the first line has two numbers.
		* The first is the number of obstacles on the x, y graph
		* The second is the number of moves the turtle will make.
	* The remainder of the lines are either moves or obstacles
	* Obstacles are lines that start with a number
		* In the performance task all of the obstacles were listed immediately after the first line but the spec did not say that would be the case and my OCaml code accounted for obstacles existed throughout the
	* Moves start with a letter
		* M -> Move. Followed by a number
		* L -> Turn left
		* N -> Turn right

## Constraints
	* There may not be more than 10,000 moves. Any moves after move 10,000 should be ignored
	* There may not be more than 10 obstacles. Any obstacle after
	* Implied: the number of obstacles and moves in the header line could differ from what is in the file, but in fact that was not the face in either the sample or actual file.
