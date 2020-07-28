fib(0, 1).
fib(1, 1).

fib(N, F) :-
        N > 1,
        N1 is N-1,
        N2 is N-2,
        N1 = 0, F1 = 1,
        N2 = 1, F2 = 1,
        F is F1+F2.

fib(N, F) :-
        N > 1,
        N1 is N-1,
        N2 is N-2,
        N1 = 1, F1 = 1,
        N2 > 1,
        NN1 is N2-1,
        NN2 is N2-2,
        NN1 = 0, FF1 = 1,
        NN2 = 1, FF2 = 1,
        F2 is FF1+FF2,
        F is F1+F2.

fib(N, F) :-
        N > 1,
        N1 is N-1,
        N2 is N-2,
        N1 > 1,
        NN1 is N1-1,
        NN2 is N1-2,
        NN1 = 0, FF1 = 1,
        NN2 = 1, FF2 = 1,
        F1 is FF1+FF2,
        N2 > 1,
        NNN1 is N2-1,
        NNN2 is N2-2,
        NNN1 = 1, FFF1 = 1,
        NNN2 > 1,
        NNNN1 is NNN2-1,
        NNNN2 is NNN2-2,
        NNNN1 = 0, FFFF1 = 1,
        NNNN2 = 1, FFFF2 = 1,
        FFF2 is FFFF1+FFFF2,
        F2 is FFF1 + FFF2,
        F is F1+F2.

:-inputs([@x : private int]).
:-outputs([Y]).
fib(@x,Y)?
