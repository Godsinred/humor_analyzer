% list of facts
dog(john).
dog(ben).
cat(ruth).

% list of rules
animal(X) :- cat(X).
animal(X) :- dog(X).



% below this will be the testing to see if we can get humor to work
