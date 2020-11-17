:-type(sells(@seller:private string, @item_seller:private string, @price_seller:private int)).
:-type(buys(@buyer :private string, @item_buyer:private string, @price_buyer:private int)).

bargain(X,Y,Z) :-
    sells(Y,Z,P1),
    buys(X,V,P2),
    Z = V,
    P1 =< P2.

:-outputs([Y]).
?-bargain('alice',Y,'garlic').
