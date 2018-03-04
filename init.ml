(*

File: init.ml
Author: Brandon Hattaway

handles initializing the board.

*)

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
