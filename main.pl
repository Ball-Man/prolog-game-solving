:- use_module(game).
:- use_module(andor).

%% :- trace.
?- next(nim(us, [1, 2]), Pos), write(Pos).
