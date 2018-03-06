(*

File: t01.ml
Author: Brandon Hattaway

(the main file)

nxn tic tac toe, player vs the computer, where the computer will have the
capability to look ahead one move to prevent the human from winning, or
will win itself.

the program starts by asking for input from the user;

the first input is n, which decides the board size (nxn)

the following inputs place holes on the board, 
where the first number in each pair is the row index,
and the second number in each pair is the column index.

holes do not belong to either player. neither player can place a piece
at a hole. it does not count towards any wins.

this list of holes is terminated with a -1

example: an input of
4
0
3
1
2
-1

will yield a 4x4 board with holes at (0,3) (the upper right corner)
and (1,1), full starting board shown below:
 | | |@
-+-+-+-
 |@| |
-+-+-+-
 | | |
-+-+-+-
 | | |

for a regular, 3x3 game board with no holes, input
3
-1

each player may win by filling an entire row, column, or diagonal with
his/her pieces.

*)

#use "init.ml";;
#use "print.ml";;
#use "play.ml";;

(* this is where the program begins *)

let main = fun () ->
	(* get the board size *)
	let n = read_int () in

	(* get the list of holes, as an int list. input terminated by -1 *)
	let listofholes = get_ints [] in

	(* initialize the board to all spaces, board represented by a char list list with n elements *)
	let board = init_board n in

	(* add holes to the board *)
	let holyboard = add_holes board listofholes in

	(* play the game! *)
	play holyboard n
;;

main ();;
