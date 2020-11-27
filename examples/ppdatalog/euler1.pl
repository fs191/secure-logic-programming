# Given a private database `factors` of integers,
# find the sum of all positive integers smaller than 1000, that
# are multiples of any of the values in the database.
#

:- type(fac(F:private int)).

sm(X, M, S) :-
  X < 1000,
  fac(F),
  divisible(X),
  X1 is X + 1,
  sm(X1, M1, S1),
  S = S1 + X.

:- outputs([S]).
?-sm(1, S).

