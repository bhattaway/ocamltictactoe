(*

File: ai.ml
Author: Brandon Hattaway

contains the decision making of the computer player.

priorities are the following:
	1) will win if the opportunity is immediately avaliable
	2) else, will deny the opponent if the opponent's victory is imminent
	3) else, will start from bottom right corner and place pieces in the first avaliable spot

more specifically, the AI's behavior is as follows:
	1) if the AI can win this turn, it will.
		a) it will check rows, then columns, then bottom-right to upper-left diag, then bottom-left to upper-right diag.
	2) if the human will win on the human's next turn, it will place its piece to block the human.
		a) it will check rows, then columns, then bottom-right to upper-left diag, then bottom-left to upper-right diag.
	3) it will place a piece in the first avaliable space, beginning in the bottom right corner, then searching left
		until it hits the beginning of the row, then moving up a row.


*)

#use "boardmanip.ml";;

(*
find the first avaliable spot, starting from bottom right

return value:
int list (with two elements) [row;col]
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

(*
returns the [row; col] of the location in which the given piece could win via row, beginning search from the bottom
and moving up. if no win is found, then returns [-1; -1]

this function is fully documented, the other functions are very similar, so refer back to this.

colspot is a flag of sorts. when a space is encountered on the given row, colspot is set to the column of the space.
*)
let rec winning_row_spot = fun board -> fun n -> fun piece -> fun row -> fun col -> fun colspot ->
	match col with

	(* col=0, therefore you are at the final check on the given row.  *)
	0 -> if (whatis board row col = piece) then
				(* to get to this point, you must have already encountered exactly one
					space, and all other locations must have had the given piece. *)
				[row; colspot]
			else if  (whatis board row col = ' ') && (colspot = -1) then
				(* to get here, all other locations must have been the given piece. 
					this location is the only space in the row. thus, return the current location. *)
				[row; col]
			else if (row = 0) then
				(* if the row is 0, then there are no more rows to search, since we began searching 
					at the bottom row (row n-1). return [-1; -1], signifying that no row wins exist. *)
				[-1; -1]
			else
				(* this row isn't a winner, but the row is not 0, therefore there are more rows to search. 
					move to the next row up, and reset the col and colspot values. *)
				winning_row_spot board n piece (row-1) (n-1) (-1)

	(* col is not 0, therefore this is not the final possible check for this row. *)
	|	col -> if (whatis board row col = piece) then
					(* so far, this row is good. either 0 or 1 space has been encountered. maintain the
						value of colspot (which could be -1 if no space has occured on this row yet,
						or 0<=colspot<n, if a space was encountered.) move on to the next column in this row *)
					winning_row_spot board n piece row (col-1) colspot
				else if (whatis board row col = ' ') && (colspot = -1) then
					(* a space was encountered, and colspot hasn't been set during this row yet!!!
						set the colspot to the current col, then move on to the next col in this row. *)
					winning_row_spot board n piece row (col-1) col
				else if (row = 0) then
					(* if the row is 0, then there are no more rows to search, since we began searching 
						at the bottom row (row n-1). return [-1; -1], signifying that no row wins exist. *)
					[-1; -1]
				else
					(* this row isn't a winner, but the row is not 0, therefore there are more rows to search. 
						move to the next row up, and reset the col and colspot values. *)
					winning_row_spot board n piece (row-1) (n-1) (-1)
;;

(*
returns the [row; col] of the location in which the given piece could win via row, beginning search from the bottom
and moving up. if no win is found, then returns [-1; -1]
*)
let rec winning_col_spot = fun board -> fun n -> fun piece -> fun row -> fun col -> fun rowspot ->
	match row with
	0 -> if (whatis board row col = piece) then
				[rowspot; col]
			else if  (whatis board row col = ' ') && (rowspot = -1) then
				[row; col]
			else if (col = 0) then
				[-1; -1]
			else
				winning_col_spot board n piece (n-1) (col-1) (-1)
	|	row -> if (whatis board row col = piece) then
						winning_col_spot board n piece (row-1) col rowspot
				else if (whatis board row col = ' ') && (rowspot = -1) then
						winning_col_spot board n piece (row-1) col row
				else if (col = 0) then
					[-1; -1]
				else
					winning_col_spot board n piece (n-1) (col-1) (-1)
;;

(*
returns the [row; col] of the location in which the given piece could win via row, beginning search from the bottom
and moving up. if no win is found, then returns [-1; -1]
*)
let rec winning_BR_to_UL_diag_spot = fun board -> fun n -> fun piece -> fun row -> fun rowspot ->
	match row with
	0 -> if (whatis board 0 0 = piece) then
				[rowspot; rowspot]
			else if  (whatis board 0 0 = ' ') && (rowspot = -1) then
				[0; 0]
			else
				[-1; -1]
	|	row -> if (whatis board row row = piece) then
						winning_BR_to_UL_diag_spot board n piece (row-1) rowspot
				else if (whatis board row row = ' ') && (rowspot = -1) then
						winning_BR_to_UL_diag_spot board n piece (row-1) row
				else
					[-1; -1]
;;

(*
returns the [row; col] of the location in which the given piece could win via row, beginning search from the bottom
and moving up. if no win is found, then returns [-1; -1]
*)
let rec winning_BL_to_UR_diag_spot = fun board -> fun n -> fun piece -> fun row -> fun rowspot ->
	match row with
	0 -> if (whatis board 0 (n-1) = piece) then
				[rowspot; (n-rowspot-1)]
			else if  (whatis board 0 (n-1) = ' ') && (rowspot = -1) then
				[0; (n-1)]
			else
				[-1; -1]
	|	row -> if (whatis board row (n-row-1) = piece) then
						winning_BL_to_UR_diag_spot board n piece (row-1) rowspot
				else if (whatis board row (n-row-1) = ' ') && (rowspot = -1) then
						winning_BL_to_UR_diag_spot board n piece (row-1) row
				else
					[-1; -1]
;;

(*
returns an int list containing pairs of all possible winning moves. if the given method of winning (row, col, 
or either diag) does not work, then that pair is -1; -1

e.g. if no winning moves for this piece exist, then the list will be [-1; -1; -1; -1; -1; -1; -1; -1]
*)
let winning_spots = fun board -> fun n -> fun piece ->
	((winning_row_spot board n piece (n-1) (n-1) (-1))
	@(winning_col_spot board n piece (n-1) (n-1) (-1))
	@(winning_BR_to_UL_diag_spot board n piece (n-1) (-1))
	@(winning_BL_to_UR_diag_spot board n piece (n-1) (-1))
	)
;;

(*
finds and returns the first move in the list that is not [-1; -1]
*)
let rec find_move = fun list ->
	match list with

	(* having no values in the list doesn't make sense, something went wrond with AI move selection process *)
	[] -> []

	|	x::y::ys -> if (x <> (-1)) && (y <> (-1)) then
						[x; y]
					else
						find_move ys
	
	(* having only one value in the list doesn't make sense *)
	|	x::xs -> []
;;

(*
determines the move for the robot. returns an int list with two values, [row; col]
*)
let robot_move = fun board -> fun n ->
	(*
	first, assembles a move list. 
		the beginning 8 values of the move list (meaning 4 moves) are the moves where 'O' can win (the robot player)
			first row win spot
			second col win spot
			third bottom-right to upper-left diag spot
			fourth bottom-left to upper-right diag spot
		the next 8 values (the next 4 moves) are the moves where 'X' can win (this is where 'O' will block the human player)
		the final two values (one move) is the move taken by default, i.e. neither player has imminent victory, so
			the AI will find the first vacant location, beginning in the bottom-right corner and searching leftwards,
			then upwards.
	*)
	let move_list = ((winning_spots board n 'O')
					@(winning_spots board n 'X')
					@(default_move board n (n-1) (n-1))
					) in

	(*
	finds and returns the first move in the move list that is not [-1; -1]
	*)
	find_move move_list
;;
