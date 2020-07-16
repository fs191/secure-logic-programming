buys('alice','garlic',40).
buys('bob','carrots',70).

sells('eve','garlic',30).
sells('bob','onions',40).
sells('chris','garlic',70).
sells('dave','garlic',40).

findPotentialBargain(Buyer,Seller,Product,Price1,Price2) :-
    buys(Buyer,Product,Price1),
    sells(Seller,Product,Price2).

bargain(Buyer,Seller,Product) :-
    findPotentialBargain(Buyer,Seller,Product,Price1,Price2),
    Price1 >= Price2.

:-inputs([x1,x2]).
:-outputs([Y]).
bargain(x1, Y, x2)?
