:-type(eds(employee:public string,department:public string, salary:private int)).
:-type(dm(department:public string, manager:public string)).

secureEDS(E,D,0) :-
    eds(E,D,S),
    2500 =< S.

secureEDS(E,D,S) :-
    eds(E,D,S),
    S < 2500.

viewESM(E,S,M) :-
    secureEDS(E,D,S),
    dm(D,M).

?- viewESM(X,Y,"manager1").
