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

let robot_move = fun board -> fun n ->
	default_move board n (n-1) (n-1)
;;
