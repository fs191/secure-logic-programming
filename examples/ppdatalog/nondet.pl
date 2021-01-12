:-type(cat(@name:public string, @gender:public string, @color:public string, @age:private int)).

friendly(Cat,X) :-
    cat(Cat,Gender,Color,Age),
    Gender =:= 'female',
    Color =:= 'black',
    Age > 5,
    X = 0.

friendly(Cat,X) :-
    cat(Cat,Gender,Color,Age),
    Gender =:= 'female',
    Color =:= 'black',
    Age < 10,
    X = 1.

friendly(Cat,X) :-
    cat(Cat,Gender,Color,Age),
    Gender =:= 'female',
    Color =:= 'black',
    Age =:= 7,
    X = 2.

:-outputs([Y,X]).
?-friendly(Y,X).
