:-type(par(@child:private string, @parent:private string)).

parents(X,Y) :-
  par(Z,X),
  par(Z,Y).

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
  par(X1,_X1),
  par(Y1,_Y1),
  _X1 = _Y1.

sg(X,Y) :-
  par(X,X1),
  par(Y,Y1),
  par(X1,_X1),
  par(Y1,_Y1),
  par(_X1,__X1),
  par(_Y1,__Y1),
  __X1 = __Y1.

:- inputs([@x : public string]).
:- outputs([Y]).
sg(@x,Y)?
