:-type(sells(@seller:private string, @item:private string, @price:private int)).
:-type(buys (@buyer :private string, @item:private string, @price:private int)).

bargain(X0,X1,X2) :-
    buys(X0, X2, X3),
    sells(X1, X2, X4),
    X3 >= X4.

% question: what Alice can buy for her money?
% the seller's name is a free variable
:-input([x : private string]).
:-output([Y]).
bargain(x, Y, Z)?
