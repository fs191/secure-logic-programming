:-type(eds(Employee:public string,Department:public string, Salary:private int)).
:-type(dm(Dm_department:public string, Manager:public string)).

viewESM(X0,X2,X4) :-
  eds(X0,X1,X2),
  X2 < 2500,
  dm(X3,X4),
  X1 = X3.

viewESM(X0,0,X4) :-
  eds(X0,X1,X2),
  2500 =< X2,
  dm(X3,X4),
  X1 = X3.

:- inputs([]).
:- outputs([Y1, Y2]).
?-viewESM(Y1,Y2,'manager1').
