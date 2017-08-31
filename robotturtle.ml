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

let obstacles = [(0,2)];;
let commands = [(M,Some 5);(R,None);(M,Some 1);(L,None);(M,Some 3);(L,None);(L,None);(M,Some 3)];;

let next_pos (pos : int * int) (direction : direction) = match direction with
	| N -> (match pos with (x,y) -> (x,y+1))
	| E -> (match pos with (x,y) -> (x+1,y))
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
	if List.mem (next_pos curr_pos direction) obstacles then let () = Printf.printf "Obstable!!" in curr_pos
	else move (next_pos curr_pos direction) direction (spaces - 1);;

let calculate_euclidean_distance (start_pos : position) (curr_pos : position) =
	let x1 = float_of_int (match start_pos with (x,_) -> x) in
	let y1 = float_of_int (match start_pos with (_,y) -> y) in
	let x2 = float_of_int (match curr_pos with (x,_) -> x) in
	let y2 = float_of_int (match curr_pos with (_,y) -> y) in
	sqrt (((x2 -. x1) ** 2.0) +. ((y2 -. y1) ** 2.0));;

let rec process_commands commands
	(start_pos : position) (curr_pos : position) (direction : direction) (max_dist : float) =
	match commands with
	| [] -> max_dist
	| h::t -> match h with
		| (M,spaces) ->
			let max_dist =
				if calculate_euclidean_distance start_pos curr_pos > max_dist then calculate_euclidean_distance start_pos curr_pos
				else max_dist in
				let () = Printf.printf "Current max distaces is %f\n" max_dist in
			process_commands t start_pos (move curr_pos direction (match spaces with | Some x -> x | None -> 0)) direction max_dist
		| (L,_) ->
			process_commands t start_pos curr_pos (turn direction L) max_dist
		| (R,_) ->
			process_commands t start_pos curr_pos (turn direction R) max_dist;;

let () =
	if (Array.length Sys.argv) < 2 then printf "no file specified"
	else 
	printf "Starting\n";;
