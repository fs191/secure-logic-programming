% Aggregation test

less_than(0, Y) :-
  Y >= 0.

less_than_eq(X, Y) :-
  X > 0,
  X1 is X - 1,
  Y1 is Y - 1,
  less_than_eq(X1, Y1).

:-outputs([Y]).
?-sum(less_than(X,5), X, Y).
