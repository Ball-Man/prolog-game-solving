:- module(game, [moves/3]).

% MNK (eg. tic-tac-toe) game predicates.
% An MNK game position is represented by the term:
% mnk(turn, [[x_0, ..., x_m-1]_0, ..., [...]_n-1], k), internal representation is
% basically the matrix of the MNK game. Any x_i can be either us (circle).
% them (cross) or blank.

% Move generator
% Move is a pair of integers (X, Y) that represent what cell the player wants
% to fill. X in [0, m), Y in [0, n).
moves(mnk(T, Pos, K), X-Y, mnk(T1, Next, K)) :-
  gennext(Pos, T, Next),

  % Enforce move coordinates
  nth0(Y, Next, NextRow),
  nth0(X, NextRow, NextCell),
  nth0(Y, Pos, Row),
  nth0(X, Row, Cell),
  Cell \== NextCell,

  nextturn(T, T1).

% Accept the game matrix and generates each row
gennext(L, T, L1) :- gennext(L, T, 1, L1).
gennext([], _, 0, []).
gennext([H| L], T, D, [H1| L1]) :-
  genrow(H, T, H1),
  rowdiff(H, H1, Diffs),
  Diffs =< D,
  NewD is D - Diffs,
  gennext(L, T, NewD, L1).

genrow([], _, []).
genrow([blank| L], T, [blank| L1]) :- genrow(L, T, L1).
genrow([blank| L], T, [T| L1]) :- genrow(L, T, L1).
genrow([H| L], T, [H| L1]) :- H \== blank, !, genrow(L, T, L1).

% Number of differences between two rows.
% Both lists must be fully initialized.
rowdiff([], [], 0).
rowdiff([H| L], [H1| L1], N) :- H == H1, !, rowdiff(L, L1, N).
rowdiff([H| L], [H1| L1], N) :- rowdiff(L, L1, NP), N is NP + 1.

nextturn(us, them).
nextturn(them, us).
