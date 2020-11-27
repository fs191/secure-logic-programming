:-type(db(A:private string, B:private string)).

:-outputs([A]).
:-inputs([B:public string]).

f(X, Y) :-
  db(X, Y).

?-f(A, B).
