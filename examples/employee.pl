data eds(public string employee,public string department,private int salary).
data dm(public string department, public string manager).

secureEDS(E,D,0) :-
    eds(E,D,S),
    2500 =< S.

secureEDS(E,D,S) :-
    eds(E,D,S),
    S < 2500.

viewESM(E,S,M) :-
    secureEDS(E,D,S),
    dm(D,M).
