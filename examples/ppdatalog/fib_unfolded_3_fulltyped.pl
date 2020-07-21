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
        NN1: private int is N2: private int-1: public int  : public bool,
        NN2: private int is N2: private int-2: public int  : public bool,
        NN1: private int = 0: public int                   : private bool,
        FF1: public int = 1: public int                    : public bool, 
        NN2: private int = 1: public int                   : private bool,
        FF2: public int = 1: public int                    : public bool,
        F2: public int is FF1: public int + FF2: public int : public bool,
        F: public int is F1: public int + F2: public int : public bool.

fib(N: private int, F: public int) : private bool :-
        N: private int > 1: public int                     : private bool,
        N1: private int is N: private int-1: public int    : public bool,
        N2: private int is N: private int-2: public int    : public bool,

        N1: private int > 1: public int                    : private bool,
        NN1: private int is N1: private int-1: public int  : public bool,
        NN2: private int is N1: private int-2: public int  : public bool,
        NN1: private int = 0: public int                   : private bool,
        FF1: public int = 1: public int                    : public bool,
        NN2: private int = 1: public int                   : private bool,
        FF2: public int = 1: public int                    : public bool,
        F1: public int is FF1: public int + FF2: public int : public bool,

        N2: private int > 1: public int                     : private bool,
        NNN1: private int is N2: private int-1: public int  : public bool,
        NNN2: private int is N2: private int-2: public int  : public bool,
        NNN1: private int = 1: public int                   : private bool,
        FFF1: public int = 1: public int                    : public bool,

        NNN2: private int > 1: public int                   : private bool,
        NNNN1: private int is NNN2: private int-1: public int : public bool,
        NNNN2: private int is NNN2: private int-2: public int : public bool,
        NNNN1: private int = 0: public int                    : private bool,
        FFFF1: public int = 1: public int                     : public bool,
        NNNN2: private int = 1: public int                    : private bool,
        FFFF2: public int = 1: public int                     : public bool,
        FFF2: public int is FFFF1: public int+FFFF2: public int : public bool,

        F2: public int is FFF1: public int + FFF2: public int   : public bool,

        F: public int is F1: public int + F2: public int        : public bool.

% Note that the output y is "public", but the satisfiability bit "b" is private.
% That is, privately computing fibonacci number with index 'x' in fact matches 'x' against
% pre-computed pairs [(0,1),(1,1),(2,2),(3,3),(4,5),...] and takes the corresponding 'y'.
:-inputs([@x : private int]).
:-outputs([Y]).
fib(@x : private int,Y : public int) : private bool?

