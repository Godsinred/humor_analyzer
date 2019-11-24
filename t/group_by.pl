:- use_module(library(list_util)).

prime(1).
prime(2).
prime(3).
prime(5).
prime(7).

composite(X) :-
    \+ prime(X).

divisibility(X, Y) :-
    maplist(prime, [X, Y]).

divisibility(X, Y) :-
    maplist(composite, [X, Y]).

:- use_module(library(tap)).

none :-
    group_by(==, [], X),
    X == [],
    group([], Y),
    Y == [].

none_backwards :-
    group_by(==, X, []),
    X == [],
    group(Y, []),
    Y == X.

one :-
    group_by(==, [a], X),
    X = [[a]],
    group([a], Y),
    Y = [[a]].

one_backwards :-
    group_by(==, X, [[a]]),
    X == [a],
    group(Y, [[a]]),
    Y == X.

several :-
    group_by(==, [m,i,s,s,i,s,s,i,p,p,i], X),
    X == [[m],[i],[s,s],[i],[s,s],[i],[p,p],[i]],
    group([m,i,s,s,i,s,s,i,p,p,i], Y),
    Y == [[m],[i],[s,s],[i],[s,s],[i],[p,p],[i]].

several_backwards :-
    group_by(==, X, [[m],[i],[s,s],[i],[s,s],[i],[p,p],[i]]),
    X == [m,i,s,s,i,s,s,i,p,p,i],
    group(Y, [[m],[i],[s,s],[i],[s,s],[i],[p,p],[i]]),
    Y == X.

primes_and_composites :-
    group_by(divisibility, [1,2,3,4,5,6,7], X),
    X = [[1,2,3],[4],[5],[6],[7]].

backwards_but_wrong_equality(fail) :-
    % a, b and c are not equal according to ==/2
    group_by(==, _, [[a,b,c],[d,d]]).

primes_and_composites_instantiation_error :-
    catch(
        (  group_by(divisibility, X, _Y),
           fail
        ),
        Error,
        Error = error(instantiation_error, X)
    ).
