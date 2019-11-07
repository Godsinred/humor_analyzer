joke(haha).
pun(lol).

humor(X) :- joke(X).
humor(X) :- pun(X).

:- initialization forall(humor(X), writeln(X)).
