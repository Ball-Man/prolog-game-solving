:- module(andor, [next/3, won/1]).
:- use_module(game).

% Inference engine for perfect knowledge games (full game tree, and-or
% approach).

% True if the position is won for us
won(Pos) :- terminal(Pos, us), !.
won(Pos) :-
	tomove(Pos, us), !,
	\+ terminal(Pos, them),
	moves(Pos, _, Next),
	won(Next).
won(Pos) :-
	tomove(Pos, them), !,
	\+ (moves(Pos, _, Next),
		\+ won(Next)).

% Compute next us winning move, if the position is won
next(Pos, Move, Next) :-
	tomove(Pos, us),
	moves(Pos, Move, Next),
	won(Next).
