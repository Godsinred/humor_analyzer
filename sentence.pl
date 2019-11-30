sentence("knock knock . who is there ? Nobel . Nobel who ?"). % matches all rules
sentence("knock knock . who is there ? Nobel . Incorrect who ?"). % does not match
% funny("knock knock . who is there ? Nobel Prize . Nobel Prize who ? Nobel Winner !").
% funny("knock knock . who is there ? i love . i love who ? i dont know you tell me !").

% generate all consecutive sub-lists of S in X
%
% append/3
% https://www.swi-prolog.org/pldoc/man?predicate=append/3
% param1 and param2 get combined into param3
%
% \=    equivalent to not equals
% https://www.swi-prolog.org/pldoc/man?predicate=%5C%3D/2
%
% This is pretty much appending one word to X at a time until it matches funnyFirst as long as X is not []
substring(X,S) :- append(_,T,S), append(X,_,T) , X \= [] , write('substring write: '), writeln(X).

funnyFirst([A, B, "."]) :- A=="knock", A==B. % first two words must be the same

funnySecond(["who", "is", "there", "?"]).% must match this exact list of words

% funnyThird([_, "."]). % any single word matches (followed by period)
%
funnyThird(S) :- last(S, ".").
% The | denotes a 'tail' or 'the rest of the list.'

% got this from stacked overflow
% https://stackoverflow.com/questions/16174681/how-to-delete-the-last-element-from-a-list-in-prolog
list_butlast([X|Xs], Ys) :-                 % use auxiliary predicate ...
   list_butlast_prev(Xs, Ys, X).            % ... which lags behind by one item

list_butlast_prev([], [], _).
list_butlast_prev([X1|Xs], [X0|Ys], X0) :-
   list_butlast_prev(Xs, Ys, X1).           % lag behind by one

%funnyFourth(Fourthpart, Thirdpart): first word of each part must match
% funnyFourth([A, "who", "?"|_], [A,"."]).
funnyFourth(S) :- last(S, "?"), list_butlast(S, X), last(X, "who").

checkThirdFourth(Third, Fourth) :- same(Third, Fourth).
same([], []).
same([H1|T1], [H2|T2]):-
    H1 = H2,
    same(T1, T2).

funnyMain(L) :-  substring(First,L),
  funnyFirst(First),
  append(First,Rest,L),

  writeln(''), write('funnyFirst matched: '), writeln(First),
  write('Here is the rest of the list: '), writeln(Rest), writeln(''),

  substring(Second,Rest),
  funnySecond(Second),
  append(Second,Rest2,Rest),

  writeln(''), write('funnySecond matched: '), writeln(Second),
  write('Here is the rest of the list: '), writeln(Rest2), writeln(''),

  substring(Third,Rest2),
  funnyThird(Third),
  append(Third,Rest3,Rest2),

  writeln(''), write('funnyThird matched: '), writeln(Third),
  write('Here is the rest of the list: '), writeln(Rest3),
  list_butlast(Third, NewThird),
  write("here is what we need to match: "), writeln(NewThird), writeln(''),

  substring(Fourth,Rest3),
  funnyFourth(Fourth),
  append(Fourth,Rest4,Rest3),

  writeln(''), write('funnyFourth matched: '), writeln(Fourth),
  write('Here is the rest of the list: '), writeln(Rest4),
  list_butlast(Fourth, NewFourth),
  list_butlast(NewFourth, NewNewFourth),
  write("here is what we need to match: "), writeln(NewNewFourth), writeln(''),

  writeln('Checking if Third and Fourth are the same.'),
  checkThirdFourth(NewThird, NewNewFourth),
  writeln('true'), writeln(''),

  writeln('Now comparing analyzing the rest of the list to determine humor.'),
  writeln(Rest4)
  .

% ==== function docs ====
% string_split/4
% https://www.swi-prolog.org/pldoc/man?predicate=split_string/4
% This is ideally splitting up the sentence into a list to be processed
%
% write/1
% https://www.swi-prolog.org/pldoc/man?predicate=write/1
%
% writeln/1
% https://www.swi-prolog.org/pldoc/man?predicate=writeln/1
funny(S) :- split_string(S, " ", " ", L), write("L="),writeln(L), funnyMain(L).
