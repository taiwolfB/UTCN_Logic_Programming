add(X,LS,LE,RS,RE):-RS=LS,LE=[X|RE].


inorder_dl(nil,L,L).
inorder_dl(t(K,L,R),LS,LE):-inorder_dl(L,LSL,LEL),inorder_dl(R,LSR,LER),
    LS=LSL,
    LEL=[K|LSR],
    LE=LER.
    
inorder_dl(nil,L,L).
inorder_dl(t(K,L,R),LS,LE):-inorder_dl(L,LS,[K|LT]), inorder_dl(R,LT,LE).

all_perms(L,_) :- perm(L,L1), assert(p(L1)),fail.
all_perms(_,R) :- collect_perms(R).

collect_perms([L1|R]) :- retract(p(L1)), collect_perms(R).
collect_perms([]).

il_to_dl(L, LE, LE) :-var(L), !.
il_to_dl([H|T], [H|RS], RE) :- il_to_dl(T, RS, RE).


dl_to_il(L,L,_) :- var(L).
dl_to_il([HLS|TLS],LE, [HLS|R]) :-dl_to_il(TLS, LE, R).

complete_to_dl([], LE, LE).
complete_to_dl([H|T], [H|LS], LE) :- complete_to_dl(T, LS, LE).

dl_to_complete(LS, _, []) :- var(LS), !.
dl_to_complete([HLS|TLS], LE, [HLS|R]) :- dl_to_complete(TLS,LE, R).


collect_perms([L1|R]) :- retract(p(L1)), collect_perms(R).
collect_perms([]).

all_decompositions(L, _) :- append(L1,L2,L), assert(p([L1,L2])), fail.
all_decompositions(_, R) :- collect_perms(R).

flatten_deep_list([], LS, LS).
flatten_deep_list([H|T], [H|LS], LE) :- atomic(H), !, flatten_deep_list(T,LS,LE).
flatten_deep_list([H|T],  LS, LE) :- flatten_deep_list(H, LLS, LLE), flatten_deep_list(T,RLS,RLE), 
    						LLE = RLS, 
    						LS = LLS,
    						LE = RLE.


separate_even(nil,LS, LS).
separate_even(t(K,L,R), [K|LS], LE) :-  0 is K mod 2, separate_even(L, LLS, LLE), 
    							separate_even(R, RLS, RLE),
    						    LLE = RLS, LS = LLS, LE = RLE.
separate_even(t(_,L,R), LS, LE) :- separate_even(L, LLS, LLE), separate_even(R, RLS, RLE),
    							LLE = RLS, LS = LLS, LE = RLE.

collect_between(T, _, _, LS, LS) :- var(T), !.
collect_between(nil, _, _, LS, LS).
collect_between(t(K,L,R), K1, K2,LS,LE) :-
    K > K1, K2 > K, collect_between(L, K1, K2, LLS, [K|LLE]), collect_between(R,K1,K2,RLS,RLE),
    LLE = RLS, LS = LLS, LE = RLE.
collect_between(t(_,L,R), K1, K2, LS, LE) :-
    collect_between(L, K1, K2, LLS, LLE), collect_between(R, K1, K2, RLS, RLE),
    LLE = RLS, LS = LLS, LE = RLE.










