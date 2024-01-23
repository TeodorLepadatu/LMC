/*punct(X, Y).
lista_puncte([], _, []).
lista_puncte([punct(X,Y)|T], Val, [punct(X,Y)|Lrez]):-
    Y>Val,
    lista_puncte(T,Val,Lrez).
lista_puncte([punct(X,Y)|T],Val,Lrez):-
    Y=<Val,
    lista_puncte(T,Val,Lrez).

dropN(L, R, N) :- append(R, L1, L), length(L1, N).

rmdn(Phi, Psi) :-
    eliminate_double_negations(Phi, Psi).

eliminate_double_negations(non(non(X)), Psi) :-
    eliminate_double_negations(X, Psi).
eliminate_double_negations(non(X), non(Y)) :-
    eliminate_double_negations(X, Y).
eliminate_double_negations(X, X).

descresc([_]).
descresc([X1,X2|T]):-
    X1 is X2-1,
    descresc([X2|T]).

student(Nume, Nota).
listare_studenti([], P, []).
listare_studenti([student(X,Y)|T], P, [X|T1]):-
    Y<P,
    listare_studenti(T,P,T1).
listare_studenti([student(_,Y)|T], P, T1):-
    Y>=P,
    listare_studenti(T,P,T1).

listare_studenti([student(ionel, 8), student(maria, 10), student(gabriela, 5), student(luca, 9)], 9, R).
*/
scal(_,[],[]).
scal(X,[H|T], [H1|T1]):-
    H1 is X*H,
    scal(X,T,T1).

dot([],[],0).
dot([H1|T1],[H2|T2],R):-
    dot(T1,T2,R1),
    P is H1*H2,
    R is R1+P.

mx([],0).
mx([H|T],X):-
    mx(T,Tm),
    H>=Tm,
    X is H.
mx([H|T],X):-
    mx(T,Tm),
    H<Tm,
    X is Tm.
find(X,[X|_]).
find(X,[H|T]):-
    find(X,T).
remove_duplicates([],[]).
remove_duplicates([H|T],[H1|T1]):-
    find(H,[H1|T1]),
    remove_duplicates(T,[H1|T1]).
remove_duplicates([H|T],[H1|T1]):-
    append([H1|T1],[H],New)
    remove_duplicates(T,New).

descresc([_]).
descresc([X1,X2|T]):-
    X1 is X2-1,
    descresc([X2|T]).