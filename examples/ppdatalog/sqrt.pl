length(X, Y, L) :-
  dist(0, 0, X, Y, L).

dist(X1, Y1, X2, Y2, D) :-
  D is sqrt((X1-X2)^2 + (Y1-Y2)^2).

:-outputs([L]).
length(4, 3, L)?
