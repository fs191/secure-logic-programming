% Currently attributes and constants are handled similarly.
% This might cause some issues.
:-type("db",[@x]).

f(X,Y) :-
  X = Y.

?-f(@x,x).
