:-type(sells(Seller:private string, Item:private string, Price:private int)).
:-type(buys(Buyer :private string, Item:private string, Price:private int)).

findPotentialBargain(Buyer,Seller,Product,Price1,Price2) :-
    buys(Buyer,Product,Price1),
    sells(Seller,Product,Price2).

bargain(Buyer,Seller,Product) :-
    findPotentialBargain(Buyer,Seller,Product,Price1,Price2),
    Price1 >= Price2.

:-inputs([X1]).
:-outputs([Y]).
?-bargain(X1, Y, X2).
