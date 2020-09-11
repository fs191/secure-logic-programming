f(X1,Y1,X2,Y2,D) :-
  D = sqrt((X1-X2)^2+(Y1-Y2)^2).

:-outputs([D]).
?-f(1,2,4,6,D).
