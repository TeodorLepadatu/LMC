/*
Varianta 2
Grupa 151
Lepădatu Teodor
*/

%1
removeEven([], []).      %daca nu avem niciun element, nu avem ce sa scoatem din lista
removeEven([_], []).     %daca avem un singur element, atunci il scoatem, deoarece are indicele 0
removeEven([_,H|T], [H|Result]) :-     %daca avem mai mult de un element, adica un numar de elemente mai mare sau egal cu 2, atunci vom pune in lista rezultat al doilea element, cel de pe pozitia 1
    removeEven(T, Result).             %eliminam primele doua elemente din lista initiala, inserand elementul de pe pozitia 1 si parcurgem si lista rezultat pentru a putea pune alte elemente in ea.

/*
1.
removeEven([a,b,c,d,e,f,g],R).
R = [b, d, f] 
2.
removeEven([0,1,2,3,4,5,6,7,8,9],L).
L = [1, 3, 5, 7, 9].
*/

%2
p(Xi,Yi).     %definitia unui punct
mergePts([],[],[]).     %daca avem 0 elemente in ambele liste, rezultatul va fi lista vida
mergePts(L1,[],L1).     %daca avem elemente doar in prima lista, rezultatul va fi doar prima lista
mergePts([],L2,L2).     %daca avem elemente doar in a doua lista, rezultatul va fi doar a doua lista
mergePts([p(X1,Y1)|T1],[p(X2,Y2)|T2],[p(X1,Y1)|Trez]):-     %vrem sa punem in rezultat un punct din prima lista
    Y1<Y2,       %verificam daca ordonata primului punct este mai mica decat ordonata celui de-al doilea
    mergePts(T1,[p(X2,Y2)|T2],Trez).    %daca if-ul precedent este adevarat, punem in lista finala punctul din prima lista si pastram punctul din a doua lista.
mergePts([p(X1,Y1)|T1],[p(X2,Y2)|T2],[p(X2,Y2)|Trez]):-     %vrem sa punem in rezultat un punct din a doua lista
    Y1>=Y2,       %verificam daca ordonata primului punct este mai mare decat ordonata celui de-al doilea
    mergePts([p(X1,Y1)|T1],T2,Trez).    %%daca if-ul precedent este adevarat, punem in lista finala punctul din a doua lista si pastram punctul din prima lista.

/*
1.
mergePts([p(1,2),p(2,3),p(3,4)],[p(2,1),p(3,2),p(4,3),p(4,5)],X).
X = [p(2, 1), p(3, 2), p(1, 2), p(4, 3), p(2, 3), p(3, 4), p(4, 5)] 

2.
mergePts([p(1,1),p(2,2),p(4,5)],[p(2,2),p(5,4)],R).
R = [p(1, 1), p(2, 2), p(2, 2), p(5, 4), p(4, 5)] 
*/

%3
noDuplicateVar(Var) :-
    atom(Var).           %daca este o variabila singura 

noDuplicateVar(neg(Phi)) :-
    noDuplicateVar(Phi).        %daca avem negatie, nu se modifica numarul de variabile

noDuplicateVar(si(Phi, Psi)) :-     %daca avem si
    noDuplicateVar(Phi),            %parcurgem formula
    noDuplicateVar(Psi).            %parcurgem formula

noDuplicateVar(imp(Phi, Psi)) :-    %daca avem implica
    noDuplicateVar(Phi),            %parcurgem formula
    noDuplicateVar(Psi).            %parcurgem formula

noDuplicateVar(sau(Phi, Psi)) :-    %daca avem sau
    noDuplicateVar(Phi),            %parcurgem formula
    noDuplicateVar(Psi).            %parcurgem formula

/*
1.
noDuplicateVar(imp(a, b)).
true.

2.
noDuplicateVar(si(a, b)).
true.
*/