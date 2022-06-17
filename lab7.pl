append([], L, L).
append([H|T], L, [H|R]):-append(T, L, R).

inorder(t(K,L,R), List):-inorder(L,LL), write(K),inorder(R, RR), append(LL, [K|RR],List).
inorder(nil, []). 

hang(nil, R, R) :- !.
hang(T, nil, T) :-! .
hang(T, t(K,nil,R), t(K, T, R)) :- !.
hang(T, t(K, L, R), t(K, NL, R)) :- hang(T, L, NL).

delete_key(Key, nil, nil):-write(Key), write(' not in tree\n').
delete_key(Key, t(Key, L, nil), L):-!. % this clause covers also case for leaf(L=nil)
delete_key(Key, t(Key, nil, R), R):-!.
delete_key(Key, t(Key, L, R), R1):-!, hang(L,R,R1).
delete_key(Key, t(K, L, R), t(K, NL, R)):-Key<K, !, delete_key(Key, L, NL).
delete_key(Key, t(K, L, R), t(K, L, NR)):- delete_key(Key, R, NR). 


collect_leaves(t(K,nil,nil), [K|_]).
collect_leaves(t(_,L,R), Rez) :- collect_leaves(L, R1), collect_leaves(R, R2), append(R1,R2,Rez).
collect_leaves(nil, []).


max(A,B, A) :- A > B, !.
max(_, B, B).
max(A,_,A).

height(nil, 0).
height(t(_, L, R), H):-height(L, H1), height(R, H2), max(H1, H2, H3),H is H3+1. 



diameter(nil,0).
diameter(t(_,L,R), D) :- diameter(L,D1), diameter(R,D2), height(L,H1), height(R, H2), 
    max(D1, D2, DMAX), H3 is H1 + H2 + 1, max(DMAX, H3, D).

collect_by_depths(nil, _,_,[]).
collect_by_depths(t(K,_,_,_), D, D, [K|_]).
collect_by_depths(t(_,L,M,R), D, CD, Rez) :- D1 is CD + 1, 
 	collect_by_depths(L, D, D1, R1),
    collect_by_depths(M, D, D1, R2),
    collect_by_depths(R, D, D1, R3),
    append(R1,R2,R4), append(R3,R4,Rez).

mirror(nil,nil).
mirror(t(_,L1,R1), t(_,L2,R2)) :- mirror(L1, R2), mirror(R1, L2).

symmetric(nil).
symmetric(t(_,L1,R1)) :- mirror(L1,R1).

    


