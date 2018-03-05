(*

File: t01.ml
Author: Brandon Hattaway

(the main file)

nxn tic tac toe, player vs the computer, where the computer will have the
capability to look ahead one move to prevent the human from winning, or
will win itself.

*)

#use "init.ml";;
#use "print.ml";;
#use "boardmanip.ml";;
#use "play.ml";;

let main = fun () ->
	let n = read_int () in

	let listofholes = get_ints [] in

	let board = init_board n in

	let holyboard = add_holes board listofholes in

	play holyboard n
;;

main ();;
