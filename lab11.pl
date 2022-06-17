edge(a, b).
edge(a, c).
edge(b, d).
edge(b, e).
edge(c, f).
edge(c, g).

:- dynamic vert/1.
:- dynamic q/1.

% DFS
d_search(X, _) :- df_search(X, _).
d_search(_, L) :- collect_v([], L).

df_search(X, L) :-  asserta(vert(X)), edge(X, Y), \+(vert(Y)), df_search(Y, L).

collect_v(L, P) :- retract(vert(X)), !, collect_v([X|L], P).
collect_v(L, L).

% BFS

do_bfs(X, Path) :-  assert(vert(X)), assert(q(X)), bfs(Path).

bfs(Path) :- q(X), !, expand(X), bfs(Path).
bfs(Path) :- collect_v([], Path).

expand(X) :- edge(X,Y), \+(vert(Y)), assert(vert(Y)), assert(q(Y)), fail.
expand(X) :- retract(q(X)), !.

% Best FS
pos_vec(start, 0, 2, [a, d]).
pos_vec(a, 2, 0, [start, b]).
pos_vec(b, 5, 0, [a, c, end]).
pos_vec(c, 10, 0, [b,end]).
pos_vec(d, 3, 4, [start, e]).
pos_vec(e, 7, 4, [d]).
pos_vec(target, 7, 2, [b, c]).
is_target(end).

dist(Nod1, Nod2, Dist) :- 
    pos_vec(Nod1, X1, Y1, _), pos_vec(Nod2, X2, Y2, _), 
    Dist is (X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2).

order([Nod1|_], [Nod2|_]) :- 
    is_target(Target), dist(Nod1, Target, Dist1), dist(Nod2, Target, Dist2),
    Dist1 < Dist2.

best([], []) :- !.
best([[Target|Rest]|_], [Target|Rest]) :-
    is_target(Target), 
    !.
best([[H|T]|Rest], Best) :- 
    pos_vec(H, _, _, Vec),
    expand_best(Vec,[H|T],Rest,Exp),
	q(Exp,SortExp,[]),
    best(SortExp,Best).

expand_best([],_,Exp,Exp):-!.
expand_best([E|R],Cale,Rest,Exp):-
    \+(member(E,Cale)),
    !,
    expand_best(R,Cale,[[E|Cale]|Rest],Exp).
expand_best([_|R],Cale,Rest,Exp):-expand_best(R,Cale,Rest,Exp).

partition(H,[A|X],[A|Y],Z):-
    order(A,H),
    !,
    partition(H,X,Y,Z).
partition(H,[A|X],Y,[A|Z]):-partition(H,X,Y,Z).
partition(_,[],[],[]).

q([H|T],S,R):-
    partition(H,T,A,B),
    q(A,S,[H|Y]),
    q(B,Y,R).
q([],S,S).







%dfs_limited



d_search_limited(X, _, D) :- 
    df_search_limited(X, _, D).
d_search_limited(_, L, _) :- collect_v_limited([], L).

df_search_limited(X, L, D) :- 
    D > 0,
    asserta(vert(X)), 
    edge(X, Y), 
    \+(vert(Y)), 
    D1 is D - 1,
    df_search_limited(Y, L, D1).

collect_v_limited(L, P) :- retract(vert(X)), !, collect_v_limited([X|L], P).
collect_v_limited(L, L).



%bfs_limited



do_bfs_limited(X, Path, D) :- 
    assert(vert(X)), 
    assert(q(X)), 
    bfs_limited(Path, D).

bfs_limited(Path, D) :- 
    D > 0,
    q(X), 
    !,
    expand_limited(X),
    D1 is D - 1, 
    bfs_limited(Path, D1).
bfs_limited(Path,_) :- collect_v_limited([], Path).


expand_limited(X) :- 
    edge(X,Y),
    \+(vert(Y)), 
    assert(vert(Y)),
    assert(q(Y)),
    fail.
expand_limited(X) :-
    retract(q(X)), 
    !.

    
