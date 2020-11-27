% Program for testing the public private reordering optimization
:-type(ex(X: private int)).

f(X, Z, Q) :-
  ex(Y),
  ex(P),
  X = Y,
  Q = P,
  S is Z,
  X = P,
  X = Q,
  X = S.

:-outputs([X, Y]).
?-f(X, 3, Y).
