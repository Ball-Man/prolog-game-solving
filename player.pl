:- module(player, [play/1]).
:- use_module(andor).
:- use_module(game).

% Presentation layer: interact with the user, show the game state, etc.

user(us, 'AI').
user(them, 'User').

play(InitPos) :-
  name(Game),
  write(Game), write(' started'), nl,
  play_iter(InitPos).

% Play game from the given game position
play_iter(quit) :- !, write('Quitting'), nl.

play_iter(Pos) :-
  terminal(Pos, W), !,
  user(W, Winner),
  show(Pos), nl,
  write(Winner), write(' wins'), nl.

play_iter(Pos) :-
  tomove(Pos, them), !,
  show(Pos), nl,
  write('Your move: '),
  read(Move),
  % Sanity check
  (
    Move == quit, !,
    Next = quit;
    moves(Pos, Move, Next), !;
    write('Illegal move, try again'), nl,
    Next = Pos
  ),
  play_iter(Next).

play_iter(Pos) :-
  tomove(Pos, us),
  show(Pos), nl,
  next(Pos, Move, Next),
  write('AI moves '), write(Move), nl,
  play_iter(Next).
