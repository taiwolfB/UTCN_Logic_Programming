

edge(a, b).
edge(b, a).
edge(b, c).
edge(c, b).
edge(b, d).
edge(d, b).
edge(d, a).
edge(a, d).
edge(c, d).
edge(d, c).
edge(g, e).
edge(e, g).
edge(e, f). 
edge(f, e).
edge(h, h).

neighbor(a, [b,d]).
neighbor(b, [a,d,c]).
neighbor(c, [d,b]).
neighbor(d, [a,b,c]).
neighbor(g, [e]).
neighbor(e, [g,f]).
neighbor(f, [e]).
neighbor(h, []).




is_edge(X,Y):- edge(X,Y); edge(Y,X).

path(X,Y,Path):-path(X,Y,[X],Path).
path(X,Y,PPath, FPath):- is_edge(X,Z),\+(member(Z, PPath)), path(Z, Y, [Z|PPath], FPath).
path(X,X,PPath, PPath).

check_restrictions([],_):- !.
check_restrictions([H|T], [H|R]):- !, check_restrictions(T,R).
check_restrictions(T, [_|L]):-check_restrictions(T,L).


restricted_path(X,Y,LR,P):- path(X,Y,P), check_restrictions(LR, P).



new_neighbor(N,N1) :- retract(neighbor(N,L)), !, asserta(N, [N1|L]).
new_neighbor(N, N1) :- asserta(neighbor(N,[N1])).
    
    
edge_to_neighbour:- edge(N,N1), new_neigbor(N,N1), new_neighbor(N1,N), fail.
edge_to_neighbour.


better_path(X, Y, Path) :- is_edge(X,Z), better_path(Z, Y, Path), member(Z, Path),!, fail.
better_path(X, Y, Path) :-  better_path(X,Y, [X|Path]).
better_path(X, X, [X|_]).


edge_w(a, b,1).
edge_w(b, a,2).
edge_w(b, c,3).
edge_w(c, b,1).
edge_w(b, d,1).
edge_w(d, b,2).
edge_w(d, a,2).
edge_w(a, d,1).
edge_w(c, d,1).
edge_w(d, c,2).
edge_w(g, e,2).
edge_w(e, g,3).
edge_w(e, f,1). 
edge_w(f, e,1).
edge_w(h, h,2).



is_edge_w(X,Y, C):- edge_w(X,Y,C); edge_w(Y,X,C). 

optimal_path_w(X,Y,_):-asserta(sol_part([],100)),path_w(X,Y,[X],0).
optimal_path_w(_,_,Path):-retract(sol_part(Path,_)).

path_w(X,X,Path,LPath):-retract(sol_part(_,_)),
    			!,	
				asserta(sol_part(Path,LPath)),
    			fail.
path_w(X,Y,PPath,LPath):-is_edge_w(X,Z,C),
    \+(member(Z,PPath)), 
    LPath1 is LPath + C, 
    sol_part(_,Lopt),
    LPath1<Lopt,
    path(Z,Y,[Z|PPath],LPath1)

    







