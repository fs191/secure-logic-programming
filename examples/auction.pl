data sells(private string seller, private string item, private int price).
data buys(private string buyer, private string item, private int price).

bargain(X,Y,Z) :-
    sells(Y,Z,P1),
    buys(X,Z,P2),
    P1 =< P2.

?- bargain(X,Y,"garlic").
