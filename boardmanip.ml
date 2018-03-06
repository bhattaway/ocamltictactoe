(*

File: boardmanip.ml
Author: Brandon Hattaway

contains functions that handle modifying the board and finding values on the board.

*)

exception IgnoreCase;;

(*
returns the value at index i of the list
*)
let rec at = fun list -> fun i ->
	match i, list with
	0, x::xs -> x
	|	i, x::xs -> at xs (i-1)
	|	_,_ -> raise IgnoreCase
;;

(*
finds the target column and returns the character there.
*)
let find_col = fun boardrow -> fun col ->
	at boardrow col
;;

(*
finds the target row, for use in find_col
*)
let find_row = fun board -> fun row -> fun col ->
	find_col (at board row) col
;;

(*
returns the character at position row,col
*)
let whatis = fun board -> fun row -> fun col ->
	find_row board row col
;;

(*
modifies the given row to contain piece at the target column. builds a new row and returns it.
*)
let rec edit_row = fun boardrow -> fun col -> fun piece -> fun newrow ->
	match col, boardrow with
	_, [] -> newrow
	|	0, (x::xs) -> edit_row xs (col-1) piece (newrow@[piece])
	|	col, (x::xs) -> edit_row xs (col-1) piece (newrow@[x])
;;

(* 
places a piece at row,col and returns the new board

basically builds a new board by reading the old board, and places the piece at the desired location in the process

row and col act as iterators in this function
*)
let rec place_piece = fun board -> fun row -> fun col -> fun piece -> fun newboard ->
	match row, board with

	(* no more rows to process, return the accumulator (newboard) *)
	_, [] -> newboard

	(* reached the target row!!! edit the row by using edit_row, then add the modified row to the accumulator 
		and then move on to the next row *)
	|	0, (boardrow::boardrows) -> place_piece boardrows (row-1) col piece (newboard@[edit_row boardrow col piece []])

	(* haven't reached the target row yet... go to next row and add the current row to the accumulator *)
	|	row, (boardrow::boardrows) -> place_piece boardrows (row-1) col piece (newboard@[boardrow])
;;
