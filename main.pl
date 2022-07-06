:- use_module(game).
:- use_module(andor).

%% :- trace.
?- next(nim(us, [3, 5, 9]), Move, Pos), write(Move), nl, write(Pos).
