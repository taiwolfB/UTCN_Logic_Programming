append1([],R,R).
append1([H|T],L2,[H|Res]) :- append(T,L2,Res).
 
delete(X, [X|T], T). %stopping condition 1
delete(_,[], []). % stopping condition 2
delete(X,[H|T], [H|R]) :- delete(X,T,R).

delete_all(X, [X|T], T) :- delete_all(X, T, T).
delete_all(_,[],[]).
delete_all(X,[H|T], [H|R]) :- delete(X,T,R).

append3([],[],R,R).
append3([],[H2|T2],L3,[H2|Res]) :- append3([], T2, L3, Res).
append3([H1|T1],L2,L3, [H1|Res]) :- append3(T1, L2, L3, Res). 

addFirst(X,L,[X|L]).

sumElements(0,[]).
sumElements(Acc,[H|T]) :- sumElements(Acc1, T), Acc is Acc1 + H.

separate_parity([],[],[]).
separate_parity([H|T], [H|E], O) :-  0 is H mod 2, separate_parity(T, E, O).
separate_parity([H|T], E, [H|O]) :-  1 is H mod 2, separate_parity(T, E, O).

remove_duplicates([],[]).
remove_duplicates([H|T], R) :- remove_duplicates(T, R), member(H,R).
remove_duplicates([H|T], [H|R]) :- remove_duplicates(T,R), \+ member(H,R).


replace_all(_, _, [], []).
replace_all(H, NewX, [H|T], [NewX|R]) :- replace_all(H, NewX, T, R).
replace_all(X, NewX, [H|T], [H|R]) :- replace_all(X, NewX, T, R).

drop_k([], [], _,_).
drop_k([_|T], R , 1, K1) :- drop_k(T, R, K1, K1).
drop_k([H|T], [H|R], K, K1) :-  K2 is K -1, drop_k(T, R, K2, K1).





