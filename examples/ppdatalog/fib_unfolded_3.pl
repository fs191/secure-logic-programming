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
        _N1 is N2-1,
        _N2 is N2-2,
        _N1 = 0, _F1 = 1,
        _N2 = 1, _F2 = 1,
        F2 is _F1+_F2.

        F is F1+F2.

fib(N, F) :-
        N > 1,
        N1 is N-1,
        N2 is N-2,

        N1 > 1,
        _N1 is N1-1,
        _N2 is N1-2,
        _N1 = 0, _F1 = 1,
        _N2 = 1, _F2 = 1,
        F1 is _F1+_F2.

        N2 > 1,
        __N1 is N2-1,
        __N2 is N2-2,
        __N1 = 1, __F1 = 1,

        __N2 > 1,
        ___N1 is __N2-1,
        ___N2 is __N2-2,
        ___N1 = 0, ___F1 = 1,
        ___N2 = 1, ___F2 = 1,
        __F2 is ___F1+___F2.

        F2 is __F1 + __F2

        F is F1+F2.

:-input([@x : private int])
:-output([y])
fib(@x,y)?
