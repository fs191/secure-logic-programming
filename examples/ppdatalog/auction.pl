:-type(sells(Seller:private string, Item_seller:private string, Price_seller:private int)).
:-type(buys(Buyer :private string, Item_buyer:private string, Price_buyer:private int)).

bargain(X,Y,Z) :-
    sells(Y,Z,P1),
    buys(X,V,P2),
    Z = V,
    P1 =< P2.

:-outputs([Y]).
?-bargain('alice',Y,'garlic').
