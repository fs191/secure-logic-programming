% Program for testing the public private reordering optimization
:-type(ex(@x: private int)).

f(X, Z, Q) :-
  ex(Y),
  ex(P),
  ex(W),
  X = Y,
  Z is X,
  X = W,
  X = P,
  X = Q.

:-outputs([X, Y]).
?-f(X, 3, Y).
