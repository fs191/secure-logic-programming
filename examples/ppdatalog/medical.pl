symptom(fever) :-
    query('Does patient have a fever (y/n) ?').

symptom(rash) :-
    query('Does patient have a rash (y/n) ?').

symptom(headache) :-
    query('Does patient have a headache (y/n) ?').

symptom(runny_nose) :-
    query('Does patient have a runny_nose (y/n) ?').

symptom(conjunctivitis) :-
    query('Does patient have a conjunctivitis (y/n) ?').

symptom(cough) :-
    query('Does patient have a cough (y/n) ?').

symptom(body_ache) :-
    query('Does patient have a body_ache (y/n) ?').

symptom(chills) :-
    query('Does patient have a chills (y/n) ?').

symptom(sore_throat) :-
    query('Does patient have a sore_throat (y/n) ?').

symptom(sneezing) :-
    query('Does patient have a sneezing (y/n) ?').
    
symptom(swollen_glands) :-
    query('Does patient have a swollen_glands (y/n) ?').
    
hypothesis(measles) :-
    symptom(fever),
    symptom(cough),
    symptom(conjunctivitis),
    symptom(runny_nose),
    symptom(rash).

hypothesis(german_measles) :-
    symptom(fever),
    symptom(headache),
    symptom(runny_nose),
    symptom(rash).
    
hypothesis(flu) :-
    symptom(fever),
    symptom(headache),
    symptom(body_ache),
    symptom(conjunctivitis),
    symptom(chills),
    symptom(sore_throat),
    symptom(runny_nose),
    symptom(cough).    
    
hypothesis(common_cold) :-
    symptom(headache),
    symptom(sneezing),
    symptom(sore_throat),
    symptom(runny_nose),
    symptom(chills).
    
hypothesis(mumps) :-
    symptom(fever),
    symptom(swollen_glands).

hypothesis(chicken_pox) :-
    symptom(fever),
    symptom(chills),
    symptom(body_ache),
    symptom(rash).
    
:-inputs([]).
:-outputs([Disease]).
?-hypothesis(Disease).
