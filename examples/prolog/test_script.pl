:- set_prolog_flag(verbose, silent).
:- initialization(main).

main :-
    current_prolog_flag(argv, Argv),
    [Script|Xs] = Argv,
    [Script],
    goal(Xs,Ys),
    write('true'),nl,
    write(Ys),nl,
    halt(0).
main :-
    write('false'),nl,
    halt(1).
