% Currently attributes and constants are handled similarly.
% This might cause some issues.
:-type("db"(X)).

f(X,Y) :-
  X = Y.

?-f(X,x).
