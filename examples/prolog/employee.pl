eds('alice','research',2000).
eds('bob','research',2500).
eds('chris','development',1000).
eds('charlie','sales',1100).
eds('dave','development',1500).
eds('eve','development',1200).
eds('henry','development',800).
eds('henry','research',1200).
eds('peggy','research',2100).
eds('victor','research',1900).

dm('research','manager1').
dm('development','manager2').
dm('sales','manager3').

secureEDS(E,D,0) :-
    eds(E,D,S),
    2500 =< S.

secureEDS(E,D,S) :-
    eds(E,D,S),
    S < 2500.

viewESM(E,S,M) :-
    secureEDS(E,D,S),
    dm(D,M).

:-outputs([Y1,Y2]).
?-viewESM(Y1,Y2,'manager1').
