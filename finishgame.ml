(*

File: finishgame.ml
Author: Brandon Hattaway

contains functions that check for the end of the game

*)

#use "boardmanip.ml";;

let rec is_space_avaliable = fun board -> fun n -> fun row -> fun col ->
	match row, col with
	0, 0 -> (whatis board 0 0 = ' ')
	|	row, 0 -> if (whatis board row 0 = ' ') then
						true
					else
						is_space_avaliable board n (row-1) (n-1)
	|	row, col -> if (whatis board row col = ' ') then
						true
					else
						is_space_avaliable board n row (col-1)
;;

let rec is_horizontal_win = fun board -> fun n -> fun row -> fun col -> fun piece ->
	match row, col with
	0, 0 -> (whatis board row col = piece)
	|	0, col -> if (whatis board row col = piece) then
						is_horizontal_win board n 0 (col-1) piece
					else
						false
	|	row, 0 -> if (whatis board row col = piece) then
						true
					else
						is_horizontal_win board n (row-1) (n-1) piece
	|	row, col -> if (whatis board row col = piece) then
						is_horizontal_win board n row (col-1) piece
					else
						is_horizontal_win board n (row-1) (n-1) piece
;;

let rec is_vertical_win = fun board -> fun n -> fun row -> fun col -> fun piece ->
	match row, col with
	0, 0 -> (whatis board row col = piece)
	|	row, 0 -> if (whatis board row col = piece) then
						is_vertical_win board n (row-1) 0 piece
					else
						false
	|	0, col -> if (whatis board row col = piece) then
						true
					else
						is_vertical_win board n (n-1) (col-1) piece
	|	row, col -> if (whatis board row col = piece) then
						is_vertical_win board n (row-1) col piece
					else
						is_vertical_win board n (n-1) (col-1) piece
;;

(* 
	bottom right to upper left diagonal check
	(this is the case where row and column indices are the same)
*)
let rec is_BR_to_UL_diagonal_win = fun board -> fun row -> fun piece ->
	match row with
	0 -> (whatis board 0 0 = piece)
	|	row -> if (whatis board row row = piece) then
					is_BR_to_UL_diagonal_win board (row-1) piece
				else
					false
;;

let is_diagonal_win = fun board -> fun n -> fun row -> fun col -> fun piece ->
	is_BR_to_UL_diagonal_win board (n-1) piece
;;

let is_win = fun board -> fun n -> fun piece ->
	is_horizontal_win board n (n-1) (n-1) piece ||
	is_vertical_win board n (n-1) (n-1) piece ||
	is_diagonal_win board n (n-1) (n-1) piece
;;
