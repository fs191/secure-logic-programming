:-type(sells(@seller:private string, @item_seller:private string, @price_seller:private int)).
:-type(buys(@buyer :private string, @item_buyer:private string, @price_buyer:private int)).

tax('garlic', 0.0).
tax('onions', 0.1).

bargain(Buyer,Seller,Product) :-
    buys(Buyer,Product,Budget),
    sells(Seller,Product,Cost),
    tax(Product,Tax),
    Budget >= Cost * (1 + Tax).

:-inputs([@buyer:private string]).
:-outputs([Y]).
?-bargain(@buyer, Y, 'garlic').
