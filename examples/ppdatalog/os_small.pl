problem(disc_format) :-
    query('Does the computer show error cannot format').

problem(boot_failure) :-
    query('Does the computer show boot failure').

problem(bad_sector) :-
    query('Does the computer show bad sector error').

problem(cannot_read) :-
    query('Does the computer show cannot read from specified device').


fault(booting_problem) :-
    problem(bad_sector),
    problem(boot_failure).

fault(floppy_disk_unusable) :-
    problem(bad_sector),
    problem(cannot_read),
    problem(disc_format).

:-inputs([]).
:-outputs([Fault]).
?-fault(Fault).
