:-type(sells, [@seller:private string, @item_seller:private string, @price_seller:private int]).
:-type(buys, [@buyer :private string, @item_buyer:private string, @price_buyer:private int]).

findPotentialBargain(Buyer,Seller,Product,Price1,Price2) :-
    buys(Buyer,Product,Price1),
    sells(Seller,Product,Price2).

bargain(Buyer,Seller,Product) :-
    findPotentialBargain(Buyer,Seller,Product,Price1,Price2),
    Price1 >= Price2.

:-inputs([@x1:public string]).
:-outputs([Y]).
?-bargain(@x1, Y, X2).
