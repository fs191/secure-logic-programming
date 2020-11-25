% Predicate called with the same variable in bot arguments

f(X, Y) :-
  f(X, X),
  Y >= X.

?-f(1, 3).

