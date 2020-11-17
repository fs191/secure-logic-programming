% IDB and EDB rules with the same name
:-type(f,[@x: private int]).

f(X) :-
  X = hello.

?-f(hello).
