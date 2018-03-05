(*

File: finishgame.ml
Author: Brandon Hattaway

contains functions that handle the end of the game,
including checking to see if the game ends

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
	

let is_win = fun board -> fun n -> fun piece ->
	is_horizontal_win board n (n-1) (n-1) piece
	(* 
	|| is_vertical_win || is_diagonal_win
	*)
;;
