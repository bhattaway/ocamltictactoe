(*

File: play.ml
Author: Brandon Hattaway

defines how turns work and how playing the game occurs

*)

#use "boardmanip.ml";;
#use "finishgame.ml";;
#use "ai.ml";;

(* 
determines if a given row,col is in bounds.

parameters: 
n: int (the board size)
row: int (the row in question)
col: int (the col in question)

returns a bool
*)
let is_in_bounds = fun n -> fun row -> fun col ->
	((0 <= row) && (row < n)) && ((0 <= col) && (col < n))
;;

(*
determines if a given row,col is a valid location to place a piece

checks if the location is in bounds, and if the char at that location is ' '

parameters:
board: char list list
n: int
row: int
col: int

returns a bool
*)
let valid_location = fun board -> fun n -> fun row -> fun col ->
	(is_in_bounds n row col) && (whatis board row col = ' ')
;;

(*
asks the user for two ints, one for the wanted row index, the second for the wanted col index

if the location is invalid, then the program will prompt the human again

parameters:
board: char list list
n: int (board size)

return value: int list (with two elements.) 
					(technically it doesn't matter if there are more than two elements,
					but only the first two elements in the list will be processed)
*)
let rec get_human_input = fun board -> fun n ->
	let _ = Printf.printf("row: ") in
	let row = read_int () in

	let _ = Printf.printf("col: ") in
	let col = read_int () in
	if valid_location board n row col then
		[row; col]
	else
		get_human_input board n
;;

(*
handles doing the turns. mutual recursion, and the turns alternate between
player 1 and player 2 (in this case, human and robot).

parameters:
board: char list list
n: int

return value:
	() 
	(due to printing a string at end of game)
*)
let rec do_human_turn = fun board -> fun n ->
	(* print the board *)
	let _ = print_board board n in
	let _ = Printf.printf("\nhuman's turn\n") in

	(* get input from human, returns a int list with 2 elements *)
	let human_input = get_human_input board n in

	(* dissect the input, placing row and col into their own names *)
	let row = at human_input 0 in
	let col = at human_input 1 in

	(* modify the current board, placing the player's piece at the specified location
		(location was already verified to be correct in get_human_input) *)
	let newboard = place_piece board row col 'X' [] in

	(* determine if this new board is a winning state for this player *)
	if is_win newboard n 'X' then
		let _ = print_char ('\n') in
		let _ = print_board newboard n in
		Printf.printf("human wins\n")
	(* else, check if there is more empty space avaliable. if so, move on to the next turn. *)
	else if is_space_avaliable newboard n (n-1) (n-1) then
		do_robot_turn newboard n
	(* if no space is avaliable, end the game in a draw *)
	else
		let _ = print_char ('\n') in
		let _ = print_board newboard n in
		Printf.printf("it's a draw\n")

and

(*
handles the robot's turn
*)
do_robot_turn = fun board -> fun n ->

	(* print the board *)
	let _ = print_board board n in
	let _ = Printf.printf("\nOCAML's turn\n") in

	(* get input from robot, returns a int list with 2 elements *)
	let robot_input = robot_move board n in

	(* dissect the input, placing row and col into their own names *)
	let row = at robot_input 0 in
	let col = at robot_input 1 in

	(* modify the current board, placing the robot's piece at the specified location
		(location was already verified to be correct during the move selection process) *)
	let newboard = place_piece board row col 'O' [] in

	(* determine if this new board is a winning state for the robot *)
	if is_win newboard n 'O' then
		let _ = print_char ('\n') in
		let _ = print_board newboard n in
		Printf.printf("OCAML wins\n")
	(* else, check if there is more empty space avaliable. if so, move on to the next turn. *)
	else if is_space_avaliable newboard n (n-1) (n-1) then
		do_human_turn newboard n
	(* if no space is avaliable, end the game in a draw *)
	else
		let _ = print_char ('\n') in
		let _ = print_board newboard n in
		Printf.printf("it's a draw\n")
;;

(* the main function that begins play 

parameters:
board: char list list
n: int (board size)

return value:
()
(because of the string printed at the end of the game)
*)
let play = fun board -> fun n ->
	(* check if space is avaliable 
		(the only time this is not true is if every location has been replaced with a hole)
	*)
	if is_space_avaliable board n (n-1) (n-1) then
		do_human_turn board n
	else
		(* this is only entered if the entire board is holes *)
		let _ = print_char ('\n') in
		let _ = print_board board n in
		Printf.printf("why did you fill the entire board with holes lol\n")
;;
