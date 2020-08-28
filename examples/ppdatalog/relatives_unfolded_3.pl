:-type(par, [@child:private string, @parent:private string]).

%we do not handle repeating variables in pattern matching yet
sg(X,Y) :-
  X = Y.

sg(X,Y) :-
  par(X,X1),
  par(Y,Y1),
  X1 = Y1.

sg(X,Y) :-
  par(X,X1),
  par(Y,Y1),
  par(X1,XX1),
  par(Y1,YY1),
  XX1 = YY1.

sg(X,Y) :-
  par(X,X1),
  par(Y,Y1),
  par(X1,XX1),
  par(Y1,YY1),
  par(XX1,XXX1),
  par(YY1,YYY1),
  XXX1 = YYY1.

:- inputs([@x : public string]).
:- outputs([Y]).
?-sg(@x,Y).
