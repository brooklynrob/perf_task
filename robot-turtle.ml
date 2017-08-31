

type direction = N | E | S | W;;

(* Open input file *)
	(* Expected format of file is
		- First row is the number of obstacles and the number of commands in Wally's route
		- Next rows are the number of commands *)
	(* Check for errors *)


let change_pos (pos : int * int) (direction : direction) = match direction with
	| N -> (match pos with (x,y) -> (x+1,y))
	| E -> (match pos with (x,y) -> (x,y+1))
	| S -> (match pos with (x,y) -> (x,y-1))
	| W -> (match pos with (x,y) -> (x-1,y));;

let rec move (curr_pos : int * int) (direction : direction) (spaces : int) = match spaces with
	| 0 -> curr_pos
	| _ -> move (change_pos curr_pos direction) direction (spaces - 1);;
