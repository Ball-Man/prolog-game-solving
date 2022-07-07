:- use_module(game).
%% :- use_module(andor).
%% :- use_module(player).

%% :- trace.
?- moves(mnk(us, [[blank, blank, blank], [blank, blank, blank], [blank, blank, blank], [blank, blank, blank]], 3), M, Next), write(Next), nl, write(M).
