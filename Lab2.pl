gcd(X,X,X).
gcd(X,Y,Z) :- X > Y, R is X-Y, gcd(R,Y,Z).
gcd(X,Y,Z) :- X < Y, R is Y-X, gcd(X,R,Z).



fact(0,1).
fact(N,F) :- N>0, N1 is N-1, fact(N1,F1), F is F1*N.

fact1(0,FF,FF).
fact1(N,FP,FF) :- N>0, N1 is N-1, FP1 is FP*N, fact1(N1,FP1,FF).

fact1_pretty(N,F) :- fact1(N,1,F).

forLoop(IN,IN,0).
forLoop(IN, SUM, N) :- IN1 is IN+N, N1 is N-1, forLoop(IN1, SUM, N1).


lcm(X,Y,Z) :- gcd(X,Y,GCD1), Z is X*Y/GCD1.