% Aggregation test

less_than_eq(0, Y) :-
  Y >= 0.

less_than_eq(X, Y) :-
  Y > 0,
  Y1 is Y - 1,
  less_than_eq(X1, Y1),
  Z is X1 + 1,
  X = Z.

:-outputs([Y]).
?-sum(less_than_eq(X,5), X, Y).
