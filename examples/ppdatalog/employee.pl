:-type(eds(Employee:public string,Department:public string, Salary:private int)).
:-type(dm(Dm_department:public string, Manager:public string)).

secureEDS(E,D,S0) :-
    S0 = 0,
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
