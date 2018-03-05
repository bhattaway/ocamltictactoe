(*

File: play.ml
Author: Brandon Hattaway

defines how turns work and how playing the game occurs

*)

#use "boardmanip.ml";;

let is_in_bounds = fun n -> fun row -> fun col ->
	((0 <= row) && (row < n)) && ((0 <= col) && (col < n))
;;

let valid_location = fun board -> fun n -> fun row -> fun col ->
	(is_in_bounds n row col) && (whatis board row col = ' ')
;;

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
let rec do_human_turn = fun board -> fun n ->
	let _ = check_end_of_game board n
	(* get input *)


and

do_robot_turn = fun board -> fun n ->

;;

let play = fun board -> fun n ->
	do_turn board n
;;
*)
