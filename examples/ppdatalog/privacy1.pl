:-type(db, [@a:private string, @b:private string]).

:-outputs([A]).
:-inputs([@b:public string]).

f(X, Y) :-
  db(X, Y).

?-f(A, @b).
