(*

File: init.ml
Author: Brandon Hattaway

handles initializing the board.

*)

#use "boardmanip.ml";;

let rec init_rows = fun n -> fun list ->
	match n with
	0 -> list
	|	n -> init_rows (n-1) (' '::list)
;;

let rec init_cols = fun n -> fun list -> fun acc ->
	match n with
	0 -> acc
	|	n -> init_cols (n-1) list (list::acc)
;;

let init_board = fun n ->
	init_cols n (init_rows n []) []
;;

let rec add_holes = fun board -> fun list ->
	match list with
	[] -> board
	|	x::[] -> board
	|	row::col::xs -> add_holes (place_piece board row col '@' []) xs
;;

let rec get_ints = fun list ->
	let x = read_int () in
	if x = -1 then
		list
	else
		get_ints (list@[x])
;;
