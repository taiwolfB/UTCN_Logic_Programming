deoth([],1).
depth([H|T],R):-atomic(H),!,depth(T,R).
depth([H|T],R):- depth(H,R1), depth(T,R2), R3 is R1+1,max(R3,R2,R).


flatten([],[]).
flatten([H|T], [H|R]) :- atomic(H),!, flatten(T,R).
flatten([H|T], R) :- flatten(H,R1), flatten(T,R2), append(R1,R2,R).


heads3([],[],_).
heads3([H|T],[H|R],1):-atomic(H),!,heads3(T,R,0).
heads3([H|T],R,0):-atomic(H),!,heads3(T,R,0).
heads3([H|T],R,_):-heads3(H,R1,1),heads3(T,R2,0), append(R1,R2,R).


heads_pretty(L,R) :- heads3(L, R,1).

append([], L, L).
append([H|T], L, [H|R]):-append(T, L, R).

nr_atomics([],0).
nr_atomics([H|T], R) :- atomic(H), !, nr_atomics(T,R1), R is R1 + 1.
nr_atomics([H|T], R) :- nr_atomics(H,R1), nr_atomics(T,R2), R is R1 + R2.

sum([],0).
sum([H|T], R) :- atomic(H), !, sum(T,R1), R is R1 + H.
sum([H|T], R) :- sum(H,R1), sum(T,R2), R is R1 + R2.


member1(H,[H|_]) :- !.
member1(X,[H|_]):-member1(X,H).
member1(X,[_|T]):-member1(X,T).

lasts([],[],_).
lasts([H|T], R, 0) :- atomic(H), !, lasts(T, R, 0).
lasts([H|T], R, 0) :- lasts(H, R1, 1), lasts(T, R2, 1), append(R1,R2,R).
lasts([H|T], R, 1) :- \+ atomic(H), !, lasts(H, R1,1), lasts(T,R2,1), append(R1,R2,R).
lasts([H|T], [H|_], 1) :-  T = [].

lasts_pretty(L, R) :- lasts(L, R, 0).

replace(_, _, [], []).
replace(X, X1, [X|T], [X1|R]) :- replace(X, X1, T, R).
replace(X, X1, [H|T], [H|R]) :- atomic(H), !, replace(X,X1,T,R).
replace(X, X1, [H|T], [R1|R]) :- replace(X, X1, H, R1), replace(X, X1, T, R).






       


 





