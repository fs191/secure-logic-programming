par('chris','alice').
par('chris','bob').
par('dave','eve').
par('dave','bob').
par('bob','peggy').
par('bob','victor').

parents(X,Y) :-
  par(Z,X),
  par(Z,Y).

%we do not handle repeating variables in pattern matching yet
sg(X,Y) :- X = Y.

sg(X,Y) :-
  par(X,X1),
  par(Y,Y1),
  sg(X1,Y1).

?-sg('chris','dave').
