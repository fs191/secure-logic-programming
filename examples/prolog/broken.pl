f('a', 'c', 'd').
f('a', 'c', 'c').

g('c', 'e').

h(X, Y, Z) :-
  f(X, W, Y),
  g(W, Z).

