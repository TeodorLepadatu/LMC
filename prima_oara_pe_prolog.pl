sum_ar([],0).
sum_ar([H | T], S) :- sum_ar(T, S2), S is S2+H.

/*ex2:

father_of(F,C):- male(F), parent(F,C).
mother_of(M,C):- female(M),parent(M,C).
grandmother_of(G,C):- mother_of(G,P), parent(P,C).
grandfather_of(G,C):- father_of(G,P), parent(P,C).
sister_of(S,P) :- parent(Q,S), parent(Q,P), female(S), S \= P.
brother_of(B,P):- parent(Q,B), parent(Q,P), male(B), B \= P.
aunt_of(A,P) :- sister_of(A,B), parent(B,P).
uncle_of(A,B) :- brother_of(A,C), parent(C,B).

ex3:

% not_parent(X,Y) :- not(parent(X,Y)).
not_parent(X,Y) :- (male(X); female(X)), (male(Y); female(Y)), X\=Y, not(parent(X,Y)).

ex1:

distance((A,B),(C,D),X) :- X is sqrt((C-A)**2 + (D-B)**2).

ex2:

fib(0,1).
fib(1,1).
fib(N,X) :- 2 =< N, M is N - 1, fib(M, Y), P is N - 2, fib(P, Z), X is Y + Z.

fibo(0,0,1).
fibo(1,1,1).
fibo(N,Z,X) :- 2 =< N, M is N-1, fibo(M,Y,Z), X is Y + Z.

fibg(N,X) :- fibo(N,_,X).

ex3:

line(0,_).
line(X,C):- X>0, Y is X-1, write(C), line(Y,C).

rectangle(0,_,_):-nl.
rectangle(X,Z,C):- X>0, Y is X-1, line(Z,C), nl, rectangle(Y,Z,C).
square(X,C) :- rectangle(X,X,C).

ex4:

all_a([]).
all_a([a|X]):-all_a(X).

trans_a_b([],[]).
trans_a_b([a|X],[b|Y]):-trans_a_b(X,Y).

ex5:

scalarMult(_,[],[]).
scalarMult(N,[H|T],[X|Y]) :- X is N * H, scalarMult(N,T,Y).

dot([],[],0).
dot([H|T],[X|Y],M) :- dot(T,Y,N), M is N + H * X.

max([],0).
max([H|T],X) :- max(T,Y), maxim(H,Y,X).

maxim(A,B,B) :- B>A.
maxim(A,B,A) :- A>=B.

ex1:

distance((A,B),(C,D),X) :- X is sqrt((C-A)**2 + (D-B)**2).

ex2:

fib(0,1).
fib(1,1).
fib(N,X) :- 2 =< N, M is N - 1, fib(M, Y), P is N - 2, fib(P, Z), X is Y + Z.

fibo(0,0,1).
fibo(1,1,1).
fibo(N,Z,X) :- 2 =< N, M is N-1, fibo(M,Y,Z), X is Y + Z.

fibg(N,X) :- fibo(N,_,X).

ex3:

line(0,_).
line(X,C):- X>0, Y is X-1, write(C), line(Y,C).

rectangle(0,_,_):-nl.
rectangle(X,Z,C):- X>0, Y is X-1, line(Z,C), nl, rectangle(Y,Z,C).
square(X,C) :- rectangle(X,X,C).

ex4:

all_a([]).
all_a([a|X]):-all_a(X).

trans_a_b([],[]).
trans_a_b([a|X],[b|Y]):-trans_a_b(X,Y).

ex5:

scalarMult(_,[],[]).
scalarMult(N,[H|T],[X|Y]) :- X is N * H, scalarMult(N,T,Y).

dot([],[],0).
dot([H|T],[X|Y],M) :- dot(T,Y,N), M is N + H * X.

max([],0).
max([H|T],X) :- max(T,Y), maxim(H,Y,X).

maxim(A,B,B) :- B>A.
maxim(A,B,A) :- A>=B.

ex1:

listaNelem(_,0,[]).
listaNelem(L,N,[H|T]) :- N > 0, P is N - 1, member(H,L), listaNelem(L,P,T).

ex2:

word(abalone,a,b,a,l,o,n,e).
word(abandon,a,b,a,n,d,o,n).
word(enhance,e,n,h,a,n,c,e).
word(anagram,a,n,a,g,r,a,m).
word(connect,c,o,n,n,e,c,t).
word(elegant,e,l,e,g,a,n,t).

crosswd(V1,V2,V3,H1,H2,H3) :-
                word(V1,_,A,_,B,_,C,_),
                word(V2,_,D,_,E,_,F,_),
                word(V3,_,G,_,H,_,I,_),
                word(H1,_,A,_,D,_,G,_),
                word(H2,_,B,_,E,_,H,_),
                word(H3,_,C,_,F,_,I,_).

ex3:

path(X,X,[X]).
path(X,Y,[X|L]) :- connected(X,Z), path(Z,Y,L).

pathc(X,Y) :- path(X,Y,_).

ex4:

word_letters(X,Y) :- atom_chars(X,Y).

liminus([C|L],C,L).
liminus([D|L],C,[D|M]) :- D\==C, liminus(L,C,M).

cover([],_).
cover([H|T],L) :- liminus(L,H,M), cover(T,M).

solution(Letters, Word, Len) :- word(Word), word_letters(Word,WordLetters), length(WordLetters,Len), cover(WordLetters, Letters).

search_solution(_,'no solution',0).
search_solution(ListLetters,Word,X) :- X > 0, solution(ListLetters,Word,X).
search_solution(ListLetters,Word,X) :- X > 0, not(solution(ListLetters,Word,X)), Y is X-1, search_solution(ListLetters,Word,Y).

topsolution(ListLetters,Word) :- length(ListLetters, MaxScore),  search_solution(ListLetters,Word,MaxScore).

ex1:

vars(V,[V]) :- atom(V).
vars(non(X),S) :- vars(X,S).
vars(si(X,Y),S) :- vars(X,T), vars(Y,U), union(T,U,S).
vars(sau(X,Y),S) :- vars(X,T), vars(Y,U), union(T,U,S).
vars(imp(X,Y),S) :- vars(X,T), vars(Y,U), union(T,U,S).

ex2:

val(V,[(V,A)|_],A).
val(V,[_|T],A) :- val(V,T,A).

%Solutie alternativa:

val(V,E,A) :- member((V,A),E).

ex3:

bnon(0,1). bnon(1,0).
bsi(0,0,0). bsi(0,1,0). bsi(1,0,0). bsi(1,1,1).
bsau(0,0,0). bsau(0,1,1). bsau(1,0,1). bsau(1,1,1).
% X -> Y = (non X) sau Y
bimp(X,Y,Z) :- bnon(X,NX), bsau(NX,Y,Z).

ex4:

eval(V,E,A) :- atom(V), val(V,E,A).
eval(non(X),E,A) :- eval(X,E,B), bnon(B,A).
eval(si(X,Y),E,A) :- eval(X,E,B), eval(Y,E,C), bsi(B,C,A).
eval(sau(X,Y),E,A) :- eval(X,E,B), eval(Y,E,C), bsau(B,C,A).
eval(imp(X,Y),E,A) :- eval(X,E,B), eval(Y,E,C), bimp(B,C,A).

ex5:

evals(_,[],[]).
evals(X,[E|Es],[A|As]) :- eval(X,E,A), evals(X,Es,As).

ex6:

evs([],[[]]).
evs([V|T],Es) :- evs(T,Esp), adauga(V,Esp,Es).
adauga(_,[],[]).
adauga(V,[E|T], [[(V,0)|E],[(V,1)|E]|Es]) :- adauga(V,T,Es).

ex7:

all_evals(X,As) :- vars(X,S), evs(S,Es), evals(X,Es,As).

ex8:

all_ones([]).
all_ones([1|T]) :- all_ones(T).
taut(X) :- all_evals(X,As), all_ones(As). */

% Base case: an atomic formula does not contain duplicate variables
% Case 1: a negation formula does not contain duplicate variables if its subformula does not contain duplicate variables
% Case 2: a conjunction formula does not contain duplicate variables if both of its subformulas do not contain duplicate variables
% Case 3: an implication formula does not contain duplicate variables if both of its subformulas do not contain duplicate variables
% Case 4: a disjunction formula does not contain duplicate variables if both of its subformulas do not contain duplicate variables
