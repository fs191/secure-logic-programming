% IDB and EDB rules with the same name
:-type(f(X: private int)).

f(X) :-
  X = hello.

?-f(hello).
