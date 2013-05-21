:- module(tokenizer, [getTokens/2]).

% getTokens(InList, Tokens)
getTokens([], []).
getTokens([X|Xs], Ts) :- white_space(X),getTokens(Xs,Ts). % handle whitespace seperately
getTokens(In, [T|Ts]) :- getToken(start, In, Rest, T), !, getTokens(Rest, Ts).

%
% Handle special characters and reserved words first (defined by special_char predicate)
%
getToken(start, [61,61,62|Rest], Rest, '==>'). % The production transition token of ACT-R is a multicharacter special char. Must be handled first because '=' and '>' are special characters, too.

getToken(start, [X|Xs],Xs,T) :-
  special_char(X), char_code(T,X).

%
% Handle white space to find word borders.
%

%getToken(start, [X], [], '') :-
  %white_space(X).

%getToken(start, [X|Xs], Rest, T) :-
  %white_space(X), getToken(start,Xs,Rest,T).
    
%
% Handle identifiers (everything that begins with a letter and contains only letters or numbers).
%
getToken(start, [X|Xs],Rest,T) :-
  letter(X), getToken(letters, Xs,Rest,[X],T).
  
getToken(letters, [C|R],Rest,S,T) :-
  (letter(C); digit(C); minus(C)), 
  getToken(letters,R,Rest,[C|S],T).
  
getToken(letters,R,R,S,W) :-
  reverse(S,S1),
  atom_chars(W,S1).
  
% TODO
% Handle Numbers
%

%
% Basic facts
%

white_space(X) :- X < 33.
%white_space(32).
%white_space(10).
%white_space(13).

digit(D) :- 46 < D, D < 59.

letter(L) :- 64 < L, L < 91.
letter(L) :- 96 < L, L < 123.

minus(C) :- C == 45.

special_char(33). %(
special_char(40). %(
special_char(41). %)
special_char(43). %+
special_char(45). %-
special_char(61). %=
special_char(62). %>

