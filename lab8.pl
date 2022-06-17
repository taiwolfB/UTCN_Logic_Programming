append_IL(L1, L, L) :- var(L1),!.
append_IL([H|T], L, [H|R]):- append_IL(T, L, R), !.

insert_il(X, L):-var(L), !, L=[X|_]. %found end of list, add element
insert_il(X, [X|_]):-!. %found element, stop
insert_il(X, [_|T]):- insert_il(X, T). % traverse input list to reach end/X

reverse_fwd_IL(L1, R, R) :- var(L1), !.
reverse_fwd_IL([H|T], Acc, R):-reverse_fwd_IL(T, [H|Acc], R),!.

convert_to_complete_IL(L, []) :- var(L), !.
convert_to_complete_IL([H|T], [H|R]) :- convert_to_complete_IL(T, R).
    
preorder_IL(T, _) :- var(T),!.
preorder_IL(t(K,L,R), List):-preorder_IL(L,LL), preorder_IL(R, LR), append_IL([K|LL], LR, List).

max(A, B, A):-A>B, !.
max(_, B, B).

height_IL(T, 0) :- var(T),!.
height_IL(t(_, L, R), H):- height_IL(L, H1), height_IL(R, H2), max(H1, H2, H3),H is H3+1. 


convert_to_complete_BT(T, nil) :- var(T), !.
convert_to_complete_BT(t(K,L,R), t(K,LL,RR)) :- convert_to_complete_BT(L, LL), convert_to_complete_BT(R,RR).

flatten_deep_IL(L,_) :- var(L),!.
flatten_deep_IL([H|T], [H|R]) :- atomic(H),!, flatten_deep_IL(T,R).
flatten_deep_IL([H|T], R) :- flatten_deep_IL(H,R1), flatten_deep_IL(T,R2), append_IL(R1,R2,R).


diameter_IL(T, 0) :- var(T), !.
diameter_IL(t(_,L,R), D) :- diameter_IL(L,D1), diameter_IL(R,D2), height_IL(L,H1), height_IL(R, H2), 
    max(D1, D2, DMAX), H3 is H1 + H2 + 1, max(DMAX, H3, D).

sub_IL(_, L2) :- var(L2), !, fail.
sub_IL([H1|T1], [H1|T2]) :-from_common_IL(T1, T2).
sub_IL(L1, [_|T2]) :- sub_IL(L1, T2).
    
from_common_IL(L1, _) :- var(L1),!.
from_common_IL([H1|T1], [H1|T2]):- from_common_IL(T1,T2).