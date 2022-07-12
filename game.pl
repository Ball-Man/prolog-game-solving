:- module(game, [name/1, moves/3, tomove/2, terminal/2, show/1]).

% MNK (eg. tic-tac-toe) game predicates.
% An MNK game position is represented by the term:
% mnk(turn, [[x_0, ..., x_m-1]_0, ..., [...]_n-1], k), internal representation is
% basically the matrix of the MNK game. Any x_i can be either us (circle).
% them (cross) or blank.

name('MNK').

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

% Turn selector
tomove(mnk(T, _, _), T).

% Check game termination
terminal(mnk(T, Pos, K), W) :-
  columns(Pos, Cols), rows(Pos, Rows),
  Rows0 is Rows - 1, Cols0 is Cols - 1,
  between(0, Cols0, X),
  between(0, Rows0, Y),
  (W = us; W = them),
  (
    terminal_row(Pos, Y, W, K);
    terminal_column(Pos, X, W, K);
    terminal_diags(Pos, X, 0, W, K);
    terminal_diags(Pos, 0, Y, W, K);
    terminal_diags(Pos, Cols0, Y, W, K)
  ).

% Check both sides diagonals starting at X, Y
terminal_diags(Pos, X, Y, T, K) :-
  terminal_diag(Pos, X, Y, 1, 1, T, _, M1),
  terminal_diag(Pos, X, Y, -1, 1, T, _, M2),
  T \== blank,
  (M1 >= K; M2 >= K).

% Check termination for row at index Y
terminal_row(Pos, Y, T, K) :-
  terminal_diag(Pos, 0, Y, 1, 0, T, _, M),
  T \== blank,
  M >= K.

% Check termination for column at index X
terminal_column(Pos, X, T, K) :-
  terminal_diag(Pos, X, 0, 0, 1, T, _, M),
  T \== blank,
  M >= K.

% True if player T has its stone at position X, Y in the game grid
checked(Pos, T, X, Y) :-
  nth0(Y, Pos, Row),
  nth0(X, Row, Cell),
  T = Cell.

% Query grid size
rows(Pos, Rows) :- length(Pos, Rows).
columns([], 0) :- !.
columns(Pos, Cols) :- nth0(0, Pos, Row), length(Row, Cols).

% Check if given coordinates are internal to the grid
internal(Pos, X, Y) :-
  X >= 0,
  Y >= 0,
  rows(Pos, Rows),
  Y < Rows,
  columns(Pos, Cols),
  X < Cols.

% Compute termination for diagonal starting at pos X, Y.
% Diagonal direction is given by DX and DY
% By using degenerate directions (<0, 1>, <1, 0>, inspecting rows and columns
% is possible).
% M can be left uninstantiated, it will be unified with the maximum
% chain of symbols found (that match T).
terminal_diag(Pos, X, Y, DX, DY, T, S, M) :-
  internal(Pos, X, Y),
  checked(Pos, T, X, Y), !,
  NX is X + DX, NY is Y + DY,
  terminal_diag(Pos, NX, NY, DX, DY, T, NS, NM),
  S is NS + 1,
  M is max(S, NM).

terminal_diag(Pos, X, Y, DX, DY, T, S, M) :-
  internal(Pos, X, Y),
  \+ checked(Pos, T, X, Y), !,
  NX is X + DX, NY is Y + DY,
  terminal_diag(Pos, NX, NY, DX, DY, T, NS, NM),
  M is max(NM, NS),
  S is 0.

terminal_diag(Pos, X, Y, DX, DY, T, S, M) :-
  \+ internal(Pos, X, Y),
  S is 0,
  M is 0.

% Placeholder
show(GamePos) :- write(GamePos).
