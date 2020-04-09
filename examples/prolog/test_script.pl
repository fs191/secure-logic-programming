:- set_prolog_flag(verbose, silent).
:- initialization(main).

main :-
    current_prolog_flag(argv, Argv),
    [Script|Xs] = Argv,
    [Script],
    findall(Ys, goal(Xs,Ys), Yss),
    (Yss = [] -> write('false') ; write('true')), nl,
    forall(member(Ys,Yss), (write(Ys),nl)),
    halt(0).
main :-
    write('false'),nl,
    halt(1).
