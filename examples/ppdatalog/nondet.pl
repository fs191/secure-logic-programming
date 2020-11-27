:-type(cat, [Name:public string, Gender:public string, Color:public string, Age:private int]).

friendly(Cat,X) :-
    cat(Cat,Gender,Color,Age),
    Gender =:= 'female',
    Color =:= 'white',
    Age > 5,
    X = 0.

friendly(Cat,X) :-
    cat(Cat,Gender,Color,Age),
    Gender =:= 'female',
    Color =:= 'white',
    Age < 10,
    X = 1.

friendly(Cat,X) :-
    cat(Cat,Gender,Color,Age),
    Gender =:= 'female',
    Color =:= 'white',
    Age =:= 7,
    X = 2.

:-outputs([Y,X]).
?-friendly(Y,X).
