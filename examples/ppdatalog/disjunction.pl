:-type(cat(@name:public string, @gender:public string, @color:public string, @age:private int)).

%OR is applied to two ground terms and is just an OR
friendly_female(Cat) :-
    cat(Cat,Gender,Color,Age),
    (Color =:= 'black' ; Color =:= 'tabby'),
    Gender =:= 'female',
    Age >= 5.

%OR defines a non-deterministic choice for a fresh variable
friendly_male(Cat) :-
    Gender = 'male',
    (Color = 'black' ; Color = 'tabby'),
    cat(Cat,Gender,Color,Age),
    Age >= 3.

suitable_for_adoption(Cat) :-
    friendly_female(Cat).

suitable_for_adoption(Cat) :-
    friendly_male(Cat).

:-outputs([Y]).
?-suitable_for_adoption(Y).
