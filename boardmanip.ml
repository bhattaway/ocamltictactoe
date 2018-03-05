(*

File: boardmanip.ml
Author: Brandon Hattaway

*)

let rec find_col = fun (x::xs) -> fun col ->
	match col with
	0 -> x
	|	col -> find_col xs (col-1)
;;

let rec find_row = fun (boardrow::boardrows) -> fun row -> fun col ->
	match row with
	0 -> find_col boardrow col
	|	row -> find_row boardrows (row-1) col
;;

(*
returns the character at position row,col
*)
let whatis = fun board -> fun row -> fun col ->
	find_row board row col
;;

let rec edit_row = fun boardrow -> fun col -> fun piece -> fun newrow ->
	match col, boardrow with
	_, [] -> newrow
	|	0, (x::xs) -> edit_row xs (col-1) piece (newrow@[piece])
	|	col, (x::xs) -> edit_row xs (col-1) piece (newrow@[x])
;;

let rec place_piece = fun board -> fun row -> fun col -> fun piece -> fun newboard ->
	match row, board with
	_, [] -> newboard
	|	0, (boardrow::boardrows) -> place_piece boardrows (row-1) col piece (newboard@[edit_row boardrow col piece []])
	|	row, (boardrow::boardrows) -> place_piece boardrows (row-1) col piece (newboard@[boardrow])
;;

(*
let place_piece = fun board -> fun n -> fun row -> fun col -> fun piece
	match row, col with
	0, 0 -> piece::
	*)
	
