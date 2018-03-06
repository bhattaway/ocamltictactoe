(*

File: finishgame.ml
Author: Brandon Hattaway

contains functions that check for the end of the game

*)

#use "boardmanip.ml";;

(*
determines if there are any locations on the board that are ' '

parameters:
board: char list list
n: int (board size)
row: int (should be initialized to (n-1), acts as an iterator that begins at the bottom right corner
col: int (should be initialized to (n-1), acts as an iterator that begins at the bottom right corner

return value:
bool
*)
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

(*
determines if the given piece has filled an entire horizontal row

parameters:
board: char list list
n: int (board size)
row: int (should be initialized to (n-1), acts as an iterator that begins at the bottom right corner
col: int (should be initialized to (n-1), acts as an iterator that begins at the bottom right corner
piece: char (should be 'X' or 'O')

return value:
bool
*)
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

(*
determines if the given piece has filled an entire vertical column

parameters:
board: char list list
n: int (board size)
row: int (should be initialized to (n-1), acts as an iterator that begins at the bottom right corner
col: int (should be initialized to (n-1), acts as an iterator that begins at the bottom right corner
piece: char (should be 'X' or 'O')

return value:
bool
*)
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
determines if the given piece has filled the bottom-right to upper-left diagonal
(this is the case where row and column indices are the same)

parameters:
board: char list list
row: int (should be initialized to (n-1), acts as an iterator that begins at the bottom right corner
piece: char (should be 'X' or 'O')

return value:
bool
*)
let rec is_BR_to_UL_diagonal_win = fun board -> fun row -> fun piece ->
	match row with
	0 -> (whatis board 0 0 = piece)
	|	row -> if (whatis board row row = piece) then
					is_BR_to_UL_diagonal_win board (row-1) piece
				else
					false
;;

(*
determines if the given piece has filled the bottom-left to upper-right diagonal
(this is the case where row and column indices are the different)

parameters:
board: char list list
n: int (board size)
row: int (should be initialized to (n-1), acts as an iterator that begins at the bottom right corner
piece: char (should be 'X' or 'O')

return value:
bool
*)
let rec is_BL_to_UR_diagonal_win = fun board -> fun n -> fun row -> fun piece ->
	match row with
	0 -> (whatis board 0 (n-1) = piece)
	|	row -> if (whatis board row (n-1-row) = piece) then
					is_BL_to_UR_diagonal_win board n (row-1) piece
				else
					false
;;

(*
combines the two diagonal win checks into a single function

parameters:
board: char list list
n: int (board size)
piece: char (should be 'X' or 'O')

return value:
bool
*)
let is_diagonal_win = fun board -> fun n -> fun piece ->
	is_BR_to_UL_diagonal_win board (n-1) piece ||
	is_BL_to_UR_diagonal_win board n (n-1) piece
;;

(*
determines if the given piece has attained a win

parameters:
board: char list list
n: int (board size)
piece: char (should be 'X' or 'O')

return value:
bool
*)
let is_win = fun board -> fun n -> fun piece ->
	is_horizontal_win board n (n-1) (n-1) piece ||
	is_vertical_win board n (n-1) (n-1) piece ||
	is_diagonal_win board n piece
;;
