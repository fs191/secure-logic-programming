% this rule looks simple, but implicitly it contains a matching of an input against a constant
% N = 0 for a private N (type of N is private since it is goal's input)
% the implicit matchings can be determinined similarly to matchings of database predicate arguments
fib(0 : public int, 1 : public int) : private bool.

fib(1 : public int, 1 : public int) : private bool.

% type of N is private since it is goal's input
fib(N: private int, F: public int) : private bool :-
        N: private int > 1: public int                     : private bool,
        % in an 'is' expression, we can always derive the type of LHS from type of RHS
        % since N1 is a free variable, we have an assignment and not a comparison, so bool type is public
        N1: private int is N: private int - 1: public int  : public bool,
        N2: private int is N: private int - 2: public int  : public bool,
        N1: private int = 0: public int                    : private bool,
        F1: public int  = 1: public int                    : public bool,
        N2: private int = 1: public int                    : private bool,
        F2: public int  = 1: public int                    : public bool,
        F: public int is F1: public int + F2: public int   : public bool.

fib(N: private int, F: public int) : private bool :-
        N: private int > 1: public int                     : private bool,
        N1: private int is N: private int - 1: public int  : public bool,
        N2: private int is N: private int - 2: public int  : public bool,
        N1: private int = 1: public int                    : private bool,
        F1: public int = 1: public int                     : public bool,

        N2: private int > 1: public int                    : private bool,
        _N1: private int is N2: private int-1: public int  : public bool,
        _N2: private int is N2: private int-2: public int  : public bool,
        _N1: private int = 0: public int                   : private bool,
        _F1: public int = 1: public int                    : public bool, 
        _N2: private int = 1: public int                   : private bool,
        _F2: public int = 1: public int                    : public bool,
        F2: public int is _F1: public int + _F2: public int : public bool.

        F: public int is F1: public int + F2: public int : public bool.

fib(N: private int, F: public int) : private bool :-
        N: private int > 1: public int                     : private bool,
        N1: private int is N: private int-1: public int    : public bool,
        N2: private int is N: private int-2: public int    : public bool,

        N1: private int > 1: public int                    : private bool,
        _N1: private int is N1: private int-1: public int  : public bool,
        _N2: private int is N1: private int-2: public int  : public bool,
        _N1: private int = 0: public int                   : private bool,
        _F1: public int = 1: public int                    : public bool,
        _N2: private int = 1: public int                   : private bool,
        _F2: public int = 1: public int                    : public bool,
        F1: public int is _F1: public int + _F2: public int : public bool,

        N2: private int > 1: public int                     : private bool,
        __N1: private int is N2: private int-1: public int  : public bool,
        __N2: private int is N2: private int-2: public int  : public bool,
        __N1: private int = 1: public int                   : private bool,
        __F1: public int = 1: public int                    : public bool,

        __N2: private int > 1: public int                   : private bool,
        ___N1: private int is __N2: private int-1: public int : public bool,
        ___N2: private int is __N2: private int-2: public int : public bool,
        ___N1: private int = 0: public int                    : private bool,
        ___F1: public int = 1: public int                     : public bool,
        ___N2: private int = 1: public int                    : private bool,
        ___F2: public int = 1: public int                     : public bool,
        __F2: public int is ___F1: public int+___F2: public int : public bool,

        F2: public int is __F1: public int + __F2: public int   : public bool,

        F: public int is F1: public int + F2: public int        : public bool.

% Note that the output y is "public", but the satisfiability bit "b" is private.
% That is, privately computing fibonacci number with index 'x' in fact matches 'x' against
% pre-computed pairs [(0,1),(1,1),(2,2),(3,3),(4,5),...] and takes the corresponding 'y'.
:-input([@x : private int])
:-output([Y])
fib(@x : private int,y : public int) : private bool?

