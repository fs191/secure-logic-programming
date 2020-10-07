a(X) :-
    X = 1.

b(X) :-
    X = 2.

c(X) :-
    X = 3.

result(Y) :-
    a(X1),
    b(X2),
    c(X3),
    Y is X1 + X2 + X3.

:-outputs([Y]).
?-result(Y).
