# ocamltictactoe
tic tac toe, written in ocaml, for a school project. contains basic AI.

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

the AI's behavior is as follows:
	1) if the AI can win this turn, it will.
		a) it will check rows, then columns, then bottom-right to upper-left diag, then bottom-left to upper-right diag.
	2) if the human will win on the human's next turn, it will place its piece to block the human.
		a) it will check rows, then columns, then bottom-right to upper-left diag, then bottom-left to upper-right diag.
	3) it will place a piece in the first avaliable space, beginning in the bottom right corner, then searching left
		until it hits the beginning of the row, then moving up a row.
