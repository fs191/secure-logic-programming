:-type(sells, [@buyer:private string, @item:private string, @price:private int]).
:-type(buys, [@buyer :private string, @item:private string, @price:private int]).

findPotentialBargain(Buyer,Seller,Product,Price1,Price2) :-
    buys(Buyer,Product,Price1),
    sells(Seller,Product,Price2).

bargain(Buyer,Seller,Product) :-
    findPotentialBargain(Buyer,Seller,Product,Price1,Price2),
    Price1 >= Price2.

:-inputs([@x : public string]).
:-outputs([Y]).
?-bargain(@x, Y, X2).
