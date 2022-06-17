member1(X, [X|_]):-!.
member1(X, [_|T]):-member1(X, T).

delete(X, [X|T], T):-!.
delete(X, [H|T], [H|R]):-delete(X, T, R).
delete(_, [], []).

% when reaching the empty list, unify accumulator with the free result variable
length_fwd([], Acc, Res):-Res = Acc.
% as the list is decomposed, add 1 to the accumulator; pass Res unchanged
length_fwd([_|T], Acc, Res):-Acc1 is Acc+1, length_fwd(T, Acc1, Res).


reverse([], []).
reverse([H|T], Res):-reverse(T, R1), append(R1, [H], Res).

reverse_fwd([], R, R).
reverse_fwd([H|T], Acc, R):-reverse_fwd(T, [H|Acc], R).

minimum([], M, M).
minimum([H|T], MP, M):-H<MP, !, minimum(T, H, M).
minimum([_|T], MP, M):-minimum(T, MP, M).


minimum_bwd([H], H).
minimum_bwd([H|T], M):-minimum_bwd(T, M), H>=M.
minimum_bwd([H|T], H):-minimum_bwd(T, M), H<M.


union([ ],L,L).
union([H|T],L2,R) :- member(H,L2),!,union(T,L2,R).
union([H|T],L,[H|R]):-union(T,L,R).

inters([], _, []).
inters([H|T], L2, [H|R]) :- member(H,L2), !, inters(T,L2,R). 
inters([_|T], L2, R) :- inters(T, L2, R).

set_diff([], _, []).
set_diff([H|T], L2, [H|R]) :- \+ member(H, L2), !, set_diff(T,L2,R).
set_diff([_|T], L2, R) :- set_diff(T,L2,R).

delete_min([], [], M, M).
delete_min([H|T], R, M1, M) :- H < M1, delete_min(T, R, H, M), H = M, !.
delete_min([H|T], [H|R], M1, M) :- H < M1, !, delete_min(T, R, H, M).
delete_min([H|T], [H|R], M1, M) :- delete_min(T, R, M1, M), H \= M, !.
delete_min([_|T], R, M1, M) :- delete_min(T, R, M1, M).
delete_min([H|T], R) :- delete_min([H|T], R, H, _).


delete_max([], [], MaxFinal, MaxFinal).
delete_max([H|T], R, MaxPartial, MaxFinal) :- H > MaxPartial, delete_max(T, R, H, MaxFinal), H = MaxFinal, !.
delete_max([H|T], [H|R], MaxPartial, MaxFinal) :- H > MaxPartial, !, delete_max(T, R, H, MaxFinal).
delete_max([H|T], [H|R], MaxPartial, MaxFinal) :- delete_max(T, R, MaxPartial, MaxFinal), H \= MaxFinal, !.
delete_max([_|T], R, MaxPartial, MaxFinal) :- delete_max(T, R, MaxPartial, MaxFinal).
delete_max([H|T], R) :- delete_max([H|T], R, H, _).

append([], L, L).
append([H|T], L, [H|R]):-append(T, L, R).

reverse_fromKth([], [], 0 ,_).
reverse_fromKth([H|T], R, K, K) :-  reverse(T,R1), append(R1, [H], R).
reverse_fromKth([H|T], [H|R], KPartial, K) :- K1 is KPartial + 1, reverse_fromKth(T, R, K1, K).


rotate_right_K(L, K, R) :- append(L1, L2, L), length(L2, K1), K = K1, append(L2, L1, R).


 