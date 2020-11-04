:-type(cat, [@name:public string, @gender:public string, @color:public string, @age:private int]).

%This example is similar to disjunctions.pl, but now translator should introduce disjuction itself where necessary
friendly_female(Cat) :-
    cat(Cat,Gender,Color,Age),
    Color =:= 'black',
    Gender =:= 'female',
    Age >= 5.

friendly_female(Cat) :-
    cat(Cat,Gender,Color,Age),
    Color =:= 'tabby',
    Gender =:= 'female',
    Age >= 5.

friendly_male(Cat) :-
    Gender = 'male',
    Color = 'black',
    cat(Cat,Gender,Color,Age),
    Age >= 3.

friendly_male(Cat) :-
    Gender = 'male',
    Color = 'tabby',
    cat(Cat,Gender,Color,Age),
    Age >= 3.

suitable_for_adoption(Cat) :-
    friendly_female(Cat).

suitable_for_adoption(Cat) :-
    friendly_male(Cat).

:-outputs([Y]).
?-suitable_for_adoption(Y).
