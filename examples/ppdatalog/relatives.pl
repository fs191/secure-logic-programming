:-type(par(@child:private string, @parent:private string)).

%we do not handle repeating variables in pattern matching yet
sg(X,Y) :- X = Y.

sg(X,Y) :-
  par(X,X1),
  par(Y,Y1),
  sg(X1,Z),
  Y1 = Z.

:-outputs([Y]).
?-sg('chris',Y).
