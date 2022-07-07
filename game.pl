:- module(game, [name/1, moves/3, terminal/2, tomove/2, show/1]).

% Nim game predicates (move generators, terminal positions, ...)
% A nim position is represented by the term nim(turn, [n_0, ..., n_k]) where n_i
% represents the number of matches left in row i. turn is one of ["us", "them"].

name('Nim').

% Move generator
% Move is a pair of integers (Index, Value) where:
% Index in [0, length(Pos) - 1], Value in [0, n_Index]
moves(nim(T, Pos), Index-Value, nim(T1, Next)) :-
	% Generate plausible new state
	gennext(Pos, Next),
	ldiff(Pos, Next, Diff),
	c0(Diff, Count),
	Count == 1,

	% Enforce move
	length(Pos, S),
	SizeLimit is S - 1,
	between(0, SizeLimit, Index),
	nth0(Index, Pos, PrevElement),
	nth0(Index, Next, NextElement),
	Value is PrevElement - NextElement,
	Value > 0,

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

% Pretty print the current game position
show(nim(_, Pos)) :- show_heaps(Pos, 0).

show_heaps([], _) :- !.
show_heaps([H| L], I) :-
	write(I), write('- '), show_heap(H, 'O'), nl,
	NextI is I + 1,
	show_heaps(L, NextI).

show_heap(0, _) :- !.
show_heap(N, Symbol) :-
	write(Symbol), write(' '), Next is N - 1, show_heap(Next, Symbol).
