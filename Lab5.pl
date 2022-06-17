perm(L, [H|R]):-append(A, [H|T], L), append(A, T, L1), perm(L1, R).
perm([], []).

is_ordered([_]).
is_ordered([H1, H2|T]):-H1 =< H2, is_ordered([H2|T]).

perm_sort(L, R):-perm(L,R), is_ordered(R), !.

minimum_bwd([H], H).
minimum_bwd([H|T], M):-minimum_bwd(T, M), H>=M, !.
minimum_bwd([H|_], H).

delete(X, [X|T], T):-!.
delete(X, [H|T], [H|R]):-delete(X, T, R).
delete(_, [], []).


sel_sort(L, [M|R]):- minimum_bwd(L, M), delete(M, L, L1), sel_sort(L1, R).
sel_sort([], []).

insert_ord(X, [H|T], [H|R]):-X>H, !, insert_ord(X, T, R).
insert_ord(X, T, [X|T]).

ins_sort([H|T], R):- ins_sort(T, R1), insert_ord(H, R1, R).
ins_sort([], []).


one_pass([H1, H2|T], [H2|R], F):- H1>H2, !, F = 1, one_pass([H1|T], R, F).
one_pass([H1|T], [H1|R], F):-one_pass(T, R, F).
one_pass([], [] ,_).

bubble_sort(L, R):-one_pass(L, R1, F), nonvar(F), !, bubble_sort(R1, R).
bubble_sort(L, L).

partition(Pi, [X|T], [X|Sm], Lg):-X<Pi, !, partition(Pi, T, Sm, Lg).
partition(Pi, [X|T], Sm, [X|Lg]):-partition(Pi, T, Sm, Lg).
partition(_, [], [], []).


quick_sort([H|T], R):-partition(H, T, Sm, Lg), quick_sort(Sm, SmS),
    				  quick_sort(Lg, LgS), append(SmS, [H|LgS], R).
quick_sort([], []).

%fails on emtpy list or [H] singular list
split(L, L1, L2):-length(L, Len), Len>1, K is Len/2, splitK(L, K, L1, L2).

splitK([H|T], K, [H|L1], L2):- K>0, !, K1 is K-1, splitK(T, K1, L1, L2).
splitK(T, _, [], T).

merge([H1|T1], [H2|T2], [H1|R]):-H1<H2, !, merge(T1, [H2|T2], R).
merge([H1|T1], [H2|T2], [H2|R]):-merge([H1|T1], T2, R).
merge([], L, L).
merge(L, [], L).

merge_sort(L, R):- split(L, L1, L2), merge_sort(L1, R1), 
    			   merge_sort(L2, R2),merge(R1, R2, R).
merge_sort([H], [H]).
merge_sort([], []).

delete_2(X, [X|T], T).
delete_2(X, [H|T], [H|R]):-delete_2(X, T, R).


perm1(L, [H|R]):-delete_2(H,L,R1), perm1(R1, R).
perm1([], []).

perm_sort1(L, R):-perm1(L,R), is_ordered(R), !.


maximum_bwd([H], H).
maximum_bwd([H|T], M):- maximum_bwd(T, M), H < M, !.
maximum_bwd([H|_], H).


sel_sort_descending(L, [M|R]):- maximum_bwd(L, M), delete(M, L, L1), sel_sort_descending(L1, R).
sel_sort_descending([], []).


ins_sort_fwd_util([], L, L).
ins_sort_fwd_util([H|T], R, Acc) :- insert_ord(H, Acc, Acc1), ins_sort_fwd_util(T, R, Acc1).  
    
ins_sort_fwd(L, R):- ins_sort_fwd_util(L, R, []).

bubble_sort_KPasses(L, R, K):- K > 0, K1 is K-1, one_pass(L, R1, F), nonvar(F), !, bubble_sort_KPasses(R1, R, K1).
bubble_sort_KPasses(L, L, 0).


one_pass_chars([H1, H2|T], [H2|R], F):- char_code(H1, V1), char_code(H2, V2), V1 > V2, !, F = 1, one_pass_chars([H1|T], R, F).
one_pass_chars([H1|T], [H1|R], F):-one_pass_chars(T, R, F).
one_pass_chars([], [] ,_).

bubble_sort_chars(L, R):-one_pass_chars(L, R1, F), nonvar(F), !, bubble_sort_chars(R1, R).
bubble_sort_chars(L, L).


one_pass_lengths([H1, H2|T], [H2|R], F):- length(H1, Len1), length(H2, Len2), Len1 > Len2, !, F = 1, one_pass_lengths([H1|T], R, F).
one_pass_lengths([H1|T], [H1|R], F):-one_pass_lengths(T, R, F).
one_pass_lengths([], [] ,_).

bubble_sort_lengths(L, R):-one_pass_lengths(L, R1, F), nonvar(F), !, bubble_sort_lengths(R1, R).
bubble_sort_lengths(L, L).



