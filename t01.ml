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

	let board = init_board n in

	play board n

(*
	let _ = print_board board n in
	let _ = whatis board 1 1 in
	let newboard0 = place_piece board 1 1 'X' [] in
	let _ = print_board newboard0 n in
	get_human_input newboard0 n
	*)

;;

main ();;
