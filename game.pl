% Nim game predicates (move generators, terminal positions, ...)
% A nim position is represented by the term nim([n_0, ..., n_k]) where n_i
% represents the number of matches left in row i.

% Move generator
moves(nim(Pos), nim(Next)) :-
	gennext(Pos, Next),
	ldiff(Pos, Next, Diff),
	c0(Diff, Count),
	Count == 1.

% Difference between numeric lists
% Both [H| L] and [H1| L1] must be fully instantiated
ldiff([], [], []).
ldiff([H| L], [H1| L1], R) :- ldiff(L, L1, R1), N is H - H1, R = [N| R1].


gennext([], []).
% Generate valid values for the next position (next_i in [0, pos_i])
% [H| L] must be fully instantiated
gennext([H| L], [H1| L1]) :- between(0, H, H1), gennext(L, L1).

% N unifies to the number of elements in [H| L] which are > 0.
c0([], 0).
c0([H| L], N) :- H == 0, !, c0(L, N).
c0([H| L], N) :- H > 0, !, c0(L, N1), N is N1 + 1.
