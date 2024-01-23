my_last(X, [X]).
my_last(X, [_|T]):-
    my_last(X, T).
last_but_one(X, [X,Y]).
last_but_one(X, [_|T]):-
    last_but_one(X, T).
element_at(X, [X|_], 1).
element_at(X, [_|T], K):-
    K>1,
    K1 is K-1,
    element_at(X,T,K1).
m_length(0,[]).
m_length(L,[_|T]):-
    m_length(L1,T),
    L is L1+1.
my_reverse([],[]).
my_reverse([]).

is_palindrome(L):-reverse(L,L).

dupli([],[]).
dupli([H|T], [H,H|T1]):-
    dupli(T,T1).

/*hide bito*/