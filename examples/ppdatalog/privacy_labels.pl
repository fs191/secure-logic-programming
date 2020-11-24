:-type(sells(@seller:private string, @item_seller:private string, @price_seller:private int)).
:-type(buys(@buyer :private string, @item_buyer:private string, @price_buyer:private int)).

bargain(Buyer,Seller,Product) :-
    buys(Buyer,   BoughtProduct, Price1),
    sells(Seller, SoldProduct,   Price2),
    WantedProduct = "garlic",
    Product = WantedProduct,
    Product = BoughtProduct,
    Product = SoldProduct,
    Price1 >= Price2.

:-inputs([@x1:public string]).
:-outputs([Y, Z]).
?-bargain(@x1, Y, Z).
