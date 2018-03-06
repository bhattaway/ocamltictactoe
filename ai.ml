(*

File: ai.ml
Author: Brandon Hattaway

contains the decisionmaking of the computer player.

ideally:
	1) will win if the opportunity is immediately avaliable
	2) else, will deny the opponent if the opponent's victory is imminent
	3) else, will start from bottom right corner and place pieces in the first avaliable spot

*)

#use "boardmanip.ml";;

(*
	find the first avaliable spot, starting from bottom right
*)

let rec default_move = fun board -> fun n -> fun row -> fun col ->
	match row, col with
	0, 0 -> [0; 0]
	|	row, 0 -> if (whatis board row 0 = ' ') then
						[row; col]
					else
						default_move board n (row-1) (n-1)
	|	row, col -> if (whatis board row col = ' ') then
						[row; col]
					else
						default_move board n row (col-1)
;;

let rec winning_row_spot = fun board -> fun n -> fun piece -> fun row -> fun col -> fun colspot ->
	match col with
	0 -> if (whatis board row col = piece) then
				[row; colspot]
			else if  (whatis board row col = ' ') && (colspot = -1) then
				[row; col]
			else if (row = 0) then
				[-1; -1]
			else
				winning_row_spot board n piece (row-1) (n-1) (-1)
	|	col -> if (whatis board row col = piece) then
						winning_row_spot board n piece row (col-1) colspot
					else if (whatis board row col = ' ') && (colspot = -1) then
							winning_row_spot board n piece row (col-1) col
					else if (row = 0) then
						[-1; -1]
					else
						winning_row_spot board n piece (row-1) (n-1) (-1)
;;

let winning_spots = fun board -> fun n -> fun piece ->
	(winning_row_spot board n piece (n-1) (n-1) (-1))
	(*
	@(winning_col_spot)@(winning_BR_to_UL_diag_spot)@(winning_BL_to_UR_diag_spot)
	*)
;;

let rec find_move = fun list ->
	match list with
	[] -> []
	|	x::y::ys -> if (x <> (-1)) && (y <> (-1)) then
						[x; y]
					else
						find_move ys
	|	x::xs -> []
;;

let robot_move = fun board -> fun n ->
	let move_list = ((winning_spots board n 'O')
					@(winning_spots board n 'X')
					@(default_move board n (n-1) (n-1))
					) in

	find_move move_list
;;
