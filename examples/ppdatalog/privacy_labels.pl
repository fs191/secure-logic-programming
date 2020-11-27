:-type(sells(Seller:private string, Item_seller:private string, Price_seller:private int)).
:-type(buys(Buyer :private string, Item_buyer:private string, Price_buyer:private int)).

bargain(Buyer,Seller,Product) :-
    buys(Buyer,   BoughtProduct, Price1),
    sells(Seller, SoldProduct,   Price2),
    WantedProduct = "garlic",
    Product = WantedProduct,
    Product = BoughtProduct,
    Product = SoldProduct,
    Price1 >= Price2.

:-inputs([X1:public string]).
:-outputs([Y, Z]).
?-bargain(X1, Y, Z).
