(*

File: boardmanip.ml
Author: Brandon Hattaway

*)

exception IgnoreCase;;

let rec at = fun list -> fun i ->
	match i, list with
	0, x::xs -> x
	|	i, x::xs -> at xs (i-1)
	|	_,_ -> raise IgnoreCase
;;

let find_col = fun boardrow -> fun col ->
	at boardrow col
;;

let find_row = fun board -> fun row -> fun col ->
	find_col (at board row) col
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

(* places a piece at row,col and returns the new board *)
let rec place_piece = fun board -> fun row -> fun col -> fun piece -> fun newboard ->
	match row, board with
	_, [] -> newboard
	|	0, (boardrow::boardrows) -> place_piece boardrows (row-1) col piece (newboard@[edit_row boardrow col piece []])
	|	row, (boardrow::boardrows) -> place_piece boardrows (row-1) col piece (newboard@[boardrow])
;;
