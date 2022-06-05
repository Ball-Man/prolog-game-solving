:- module(game, [moves/2, terminal/2, tomove/2]).

% Nim game predicates (move generators, terminal positions, ...)
% A nim position is represented by the term nim(turn, [n_0, ..., n_k]) where n_i
% represents the number of matches left in row i. turn is one of ["us", "them"].

% Move generator
%% moves(nim(T, Pos), nim(T, Pos)) :-
%% 	c0(Pos, 0), !.

moves(nim(T, Pos), nim(T1, Next)) :-
	gennext(Pos, Next),
	ldiff(Pos, Next, Diff),
	c0(Diff, Count),
	Count == 1,
	nextturn(T, T1).

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


nextturn(us, them).
nextturn(them, us).

% Check game termination
% If a player leaves a completely empty set of matches, they win.
terminal(nim(T, Pos), W) :- c0(Pos, 0), nextturn(T, W).

% Turn selector
tomove(nim(T, _), T).
