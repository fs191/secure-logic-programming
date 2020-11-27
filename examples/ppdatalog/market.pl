:-type(sells(Seller:private string, Item_seller:private string, Price_seller:private int)).
:-type(buys(Buyer :private string, Item_buyer:private string, Price_buyer:private int)).

findPotentialBargain(Buyer,Seller,Product,Price1,Price2) :-
    buys(Buyer,Product,Price1),
    sells(Seller,Product,Price2).

bargain(Buyer,Seller,Product) :-
    findPotentialBargain(Buyer,Seller,Product,Price1,Price2),
    Price1 >= Price2.

:-inputs([X1:public string]).
:-outputs([Y]).
?-bargain(X1, Y, X2).
