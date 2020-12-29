% Aggregation test
less_than(X, Y) :-
  Y >= 0,
  X < Y.

:-outputs([Y]).
?-sum(less_than(X,5), X, Y).
