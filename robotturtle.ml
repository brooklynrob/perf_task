open String
open Printf

type direction = N | E | S | W;;
type position = int * int;;
type obstacle = position;;
(* With more time I'd like to have cleaned up these next two lines - this is a bit hacky and not DRY *)
type turn_kinds = L | R;;
type command_kinds = L | R | M;;
type command = command_kinds * int option;;

(* this is to enforce constaints of PT *)
let max_obstacles = 10;;
let max_commands = 10000;;

(* Test Data *)
let filename_sample_test = "sample-input.txt";;
let obstacles_sample_test = [(0,2)];;
let obstacles = obstacles_sample_test;;
let commands_sample_test = [(M,Some 5);(R,None);(M,Some 1);(L,None);(M,Some 3);(L,None);(L,None);(M,Some 3)];;
let commands = commands_sample_test;;
let obstacles_and_commands = (obstacles_sample_test, commands_sample_test);;

(* Helper functon that is just a rename to clarify its use in further functions *)
let parse_line file_in = input_line file_in;;

let rec parse_file file_in =
	try
		let line_read = parse_line file_in in
		line_read :: parse_file file_in
	with
		End_of_file -> [];;

let read_file filename =
	let file_in = open_in filename in
		let parsed_dict = parse_file file_in in
		close_in file_in;
		parsed_dict;;

let process_line line : (string * string list) =
	let line_vals = String.split_on_char ' ' line in
	let line_header = List.hd line_vals in
	(line_header, line_vals);;

let process_line line : (string * string list) =
	let line_vals = String.split_on_char ' ' line in
	let line_header = List.hd line_vals in
	(line_header, line_vals);;

let rec truncate_list n xs = match n with
	0 -> []
	| _ -> List.hd xs :: truncate_list (n-1) (List.tl xs);;

let process_rest_of_file rest_of_file =
	let rec process_rest_of_file' rest_of_file (obstacles, commands) =
		match rest_of_file with
		| [] -> (obstacles, commands)
		| _ ->
		(let curr_line = process_line (List.hd rest_of_file) in
			match curr_line with
			| ("M",move_to_add) ->
				process_rest_of_file' (List.tl rest_of_file)
				(obstacles,
				(commands @ [(M, Some (int_of_string (List.nth move_to_add 1)))]))
			| ("L",_) -> process_rest_of_file' (List.tl rest_of_file) (obstacles,(commands @ [(L,None)]))
			| ("R",_) -> process_rest_of_file' (List.tl rest_of_file) (obstacles,(commands @ [(R,None)]))
			| (_,obstacle_to_add) -> process_rest_of_file' (List.tl rest_of_file) (obstacles @ [(
			int_of_string (List.nth obstacle_to_add 0), int_of_string (List.nth obstacle_to_add 1))] , commands)) in
	process_rest_of_file' rest_of_file ([],[]);;

let line_vals file_stream = String.split_on_char ' ' file_stream;;

let process_file filename : (obstacle list * command list) =
	(* Sample output of next line would be ["1 8"; "0 2"; "M 5"; "R"; "M 1"; "L"; "M 3"; "L"; "L"; "M 3"] *)
	let file_lines_as_list = read_file filename in
	(* Get first line of the file *)
	let file_header = List.hd file_lines_as_list in
	let file_header_vals = line_vals file_header in
	(* This gets to anticipate count of obstacles
		a to-do would be to check that this matches actual count of obstacles*)
	let obstacle_count = List.hd file_header_vals in
	(* This gets to anticipate count of commands
		a to-do would be to check that this matches actual count of commands *)
	let command_count = List.nth file_header_vals 1 in
	let rest_of_file = List.tl file_lines_as_list in
	let obstacles_and_commands = process_rest_of_file rest_of_file  in
	(* Constraint of PT: Obstacles are 1 <= number of obstacles <= 10 *)
	(* This test file does not have > 10 obstacles do did not add error handling to trucate obstacle list
	but it would have gone here *)
	let obstacles = match obstacles_and_commands with (obstacles,_) -> obstacles in
	let () = Printf.printf "The initial number of obstacles is %d\n" (List.length obstacles) in
	let obstacles = if (List.length obstacles) > max_obstacles
		then truncate_list max_obstacles obstacles
		else obstacles in
	let () = Printf.printf "The final number of obstacles is %d\n" (List.length obstacles) in
	let commands = match obstacles_and_commands with (_,commands) -> commands in
	let () = Printf.printf "The initial number of commands is %d\n" (List.length commands) in
	let commands = if (List.length obstacles) > max_commands
		then truncate_list max_commands commands
		else commands in
	let () = Printf.printf "The final number of commands is %d\n" (List.length commands) in
	(obstacles,commands);;


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

let check_curr_pos_ok (curr_pos : position) : bool =
	match curr_pos with (x,y) ->
		if (x > 10000) then true else
		if (y > 10000) then true else
		false;;

let rec move
	(curr_pos : int * int) (direction : direction)
	(spaces : int) (obstacles : obstacle list) =
	match spaces with
	| 0 -> curr_pos
	| _ ->
	(* Check if next space is blocked - is it is end the move *)
	if List.mem (next_pos curr_pos direction) obstacles then let () = Printf.printf "Obstacle!!\n" in curr_pos
	else move (next_pos curr_pos direction) direction (spaces - 1) obstacles;;

let calculate_euclidean_distance (start_pos : position) (curr_pos : position) =
	let x1 = float_of_int (match start_pos with (x,_) -> x) in
	let y1 = float_of_int (match start_pos with (_,y) -> y) in
	let x2 = float_of_int (match curr_pos with (x,_) -> x) in
	let y2 = float_of_int (match curr_pos with (_,y) -> y) in
	sqrt (((x2 -. x1) ** 2.0) +. ((y2 -. y1) ** 2.0));;

let rec process_commands commands
	(start_pos : position) (curr_pos : position) (direction : direction)
	(max_dist : float) (obstacles : obstacle list) =
	let () = if (check_curr_pos_ok curr_pos) then Printf.printf("Out of bounds\n") else () in
	match commands with
	| [] -> max_dist
	| h::t -> match h with
		| (M,spaces) ->
			let max_dist =
				if calculate_euclidean_distance start_pos curr_pos > max_dist then calculate_euclidean_distance start_pos curr_pos
				else max_dist in
			process_commands t start_pos
				(move curr_pos direction (match spaces with | Some x -> x | None -> 0) obstacles) direction max_dist obstacles
		| (L,_) ->
			process_commands t start_pos curr_pos (turn direction L) max_dist obstacles
		| (R,_) ->
			process_commands t start_pos curr_pos (turn direction R) max_dist obstacles;;

let get_result filename =
	let obstacles_and_commands = process_file filename in
	let obstacles = match obstacles_and_commands with (obstacles,_) -> obstacles in
	let commands = match obstacles_and_commands with (_,commands) -> commands in
	process_commands commands (0,0) (0,0) N 0. obstacles;;

let () =
	let filename = Sys.argv.(1) in
	let result = get_result filename in
	Printf.printf "The results is %f\n" result;;
