woman(ana). % Remember, predicate names are constant (start with lowercase letter)
woman(sara).
woman(ema).
woman(maria).
woman(dorina).
woman(carmen).
woman(irina).

man(andrei).
man(george).
man(alex).
man(sergiu).
man(mihai).
man(marius).

parent(maria, ana). % maria is ana’s parent
parent(george,ana). % george also is ana’s parent
parent(maria,andrei).
parent(george,andrei).
parent(dorina,maria).
parent(marius,maria).
parent(mihai,george).
parent(mihai,carmen).
parent(irina,carmen).
parent(irina, george).
parent(carmen,ema).
parent(carmen,sara).
parent(alex,sara).
parent(alex,ema).




mother(X,Y) :- woman(X), parent(X,Y).
father(X,Y) :- man(X), parent(X,Y).
sibling(X,Y) :-  parent(P,X), parent(P,Y), X \= Y.
sister(X,Y)  :-  sibling(X,Y), woman(X).
aunt(X,Y) :- sister(X,Z), parent(Z,Y).
brother(X,Y) :- sibling(X,Y), man(X).
grandmother(X,Y) :- mother(X,Z), mother(Z,Y).
grandfather(X,Y) :- father(X,Z), father(Z,Y).
ancestor(Y,Y).
ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).


