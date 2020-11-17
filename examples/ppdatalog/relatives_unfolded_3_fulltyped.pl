:-type(par(@child:private string, @parent:private string)).

% type of X is public since it is goal's input
% in this case, we could actually deduce that Y should also be a public string
% the equlaity "=" can be treated similarly to "is" from "fib.pl" example, but the free variable can now be both the LHS and the RHS
% also, more complex unifications like (X + a) = (b + Y) should first be transformed to (X = b) and (Y = b), e.g. via the unifier substitution
sg(X:public string,Y:public string) : public bool :-
  % initially, the type of Y is "unknown"
  (X:public string = Y:public string) : public bool.

% type of X is public since it is goal's input
sg(X:public string,Y:private string) : private bool :-

  % the public X is matched against private par.child, so the bool type of this line is private
  % the type of X1 is private since it gets taken from private par.parent
  par(X:public string,X1:private string) : private bool,

  % the types of (Y, Y1) is private since it gets taken from private par.child and par.parent
  % the bool type of the entire line is public since Y and Y1 are fresh variables not used before
  par(Y:private string,Y1:private string) : public bool,

  X1:private string = Y1:private string   : private bool.

sg(X:public string,Y:private string) : private bool :-
  par(X:public string,X1:private string)    : private bool,
  par(Y:private string,Y1:private string)   : public bool,
  par(X1:private string,XX1:private string) : private bool,
  par(Y1:private string,YY1:private string) : private bool,
  XX1:private string = YY1:private string   : private bool.

sg(X:public string,Y:private string) : private bool :-
  par(X:public string,X1:private string)      : private bool,
  par(Y:private string,Y1:private string)     : public bool,
  par(X1:private string,XX1:private string)   : private bool,
  par(Y1:private string,YY1:private string)   : private bool,
  par(XX1:private string,XXX1:private string) : private bool,
  par(YY1:private string,YYY1:private string) : private bool,
  XXX1:private string = YYY1:private string   : private bool.

:- inputs([@x1 : public string]).
:- outputs([Y : private string]).
?-sg(@x1:public string, Y:private string) : private bool.
