buys('alice','garlic',40).
buys('bob','carrots',70).

sells('eve','garlic',30).
sells('bob','onions',40).
sells('chris','garlic',70).
sells('dave','garlic',40).

findPotentialBargain(X_0,X_1,X_3,X_4,X_5) :-
    buys(X_0,X_3,X_4),
    sells(X_1,X_3,X_5).

bargain(X_1,X_2,X_3) :-
    findPotentialBargain(X_1,X_2,X_3,X_4,X_5),
    X_4 >= X_5.

:-inputs([X1,X2]).
:-outputs([Y]).
bargain(X1, Y, X2)?
