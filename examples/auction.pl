:-type(sells(seller:private string, item:private string, price:private int)).
:-type(buys (buyer :private string, item:private string, price:private int)).

bargain(X,Y,Z) :-
    sells(Y,Z,P1),
    buys(X,V,P2),
    Z = V,
    P1 =< P2.

?- bargain(X,Y,"garlic").
