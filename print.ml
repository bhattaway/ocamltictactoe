(*

File: print.ml
Author: Brandon Hattaway

handles printing the tictactoe board

*)

(*
handles rows that contain board data, of the form X| |O
*)
let rec print_cells = fun list ->
	match list with
	[] -> ()
	|	x::xs -> let _ = print_char (x) in
					print_bar xs

	and

	print_bar = fun list ->
	match list with
	[] -> print_char ('\n')
	|	xs -> let _ = print_char ('|') in
					print_cells xs
;;

(*
handles rows of the form -+-+-
*)
let rec print_filler_dashes = fun n ->
	match n with
	0 -> ()
	|	n -> let _ = print_char ('-') in
				print_filler_plus (n-1)
and
	print_filler_plus = fun n ->
	match n with
	0 -> print_char ('\n')
	|	n -> let _ = print_char ('+') in
				print_filler_dashes n
;;

(*
prints each row. print_row handles the rows with board data in them, (i.e. X| |O)

print_filler handles the rows inbetween (i.e. -+-+-)
*)
let rec print_row = fun board -> fun n ->
	match board with
	[] -> ()
	|	x::xs -> let _ = print_cells x in
					print_filler xs n
and
	print_filler = fun board -> fun n ->
	match board with
	[] -> ()
	|	xs -> let _ = print_filler_dashes n in
				print_row xs n
;;

let print_board = fun board -> fun n ->
	print_row board n
;;
