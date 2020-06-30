buys('alice','garlic',40).
buys('bob','carrots',70).

sells('eve','garlic',30).
sells('bob','onions',40).
sells('chris','garlic',70).
sells('dave','garlic',40).

bargain(X,Y,Z) :-
    sells(Y,Z,P1),
    buys(X,V,P2),
    Z = V,
    P1 =< P2.

:-outputs([Y]).
bargain('alice',Y,'garlic')?
