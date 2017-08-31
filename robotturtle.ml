open Printf

type direction = N | E | S | W;;
type position = int * int;;
type obstacle = position;;
type turn_kinds = L | R;;
type command_kinds = L | R | M;;
type command = command_kinds * int option;;

(* Open input file *)
	(* Expected format of file is
		- First row is the number of obstacles and the number of commands in Wally's route
		- Next rows are the number of commands *)
	(* Check for errors *)

let obstacles = [(0,1);(0,3)];;
let commands = [(M,Some 5);(R,None);(M,Some 7)];;

let next_pos (pos : int * int) (direction : direction) = match direction with
	| N -> (match pos with (x,y) -> (x+1,y))
	| E -> (match pos with (x,y) -> (x,y+1))
	| S -> (match pos with (x,y) -> (x,y-1))
	| W -> (match pos with (x,y) -> (x-1,y));;

let turn (direction : direction) (turn : turn_kinds) : direction = match direction with
	| N -> (match turn with
		| L -> W
		| R -> E)
	| E -> (match turn with
		| L -> N
		| R -> S)
	| S -> (match turn with
		| L -> E
		| R -> W)
	| W -> (match turn with
		| L -> S
		| R -> N);;

let rec move (curr_pos : int * int) (direction : direction) (spaces : int) = match spaces with
	| 0 -> curr_pos
	| _ ->
	(* Check if next space is blocked - is it is end the move *)
	if List.mem (next_pos curr_pos direction) obstacles then curr_pos
	else move (next_pos curr_pos direction) direction (spaces - 1);;

let process_commands commands =


let calculate_euclidean_distance (start_pos : position): float = 3.14;;



let () =
	if Array.length Sys.argv = 0 then printf "no file specified"
	else "Starting";;
