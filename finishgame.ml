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
