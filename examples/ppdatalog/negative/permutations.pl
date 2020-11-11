switch(X, Y, 0).

switch(X, Y, Z) :-
  X1 = X - 2,
  switch(Z, X1, Y).

switch(X, Y, Z) :-
  Y1 = Y + 1,
  switch(Y1, Z, X).

:-outputs([Z]).
?-switch(10, 10, Z).
