:- use_module(tokenizer).
:- use_module(parser).

readAll(S, []) :-
  at_end_of_stream(S).
  
readAll(S,[C|Cs]) :-
  \+ at_end_of_stream(S),
  get_code(S,C), readAll(S,Cs).

  
compile_file(F) :-
  open(F,read,S),
  readAll(S,Cs),
  compile(Cs,Structure),
  !,
  %display(Structure),
  compile_structure(Structure,Result),
  close(S).

compile(X,S) :- getTokens(X,T), parse(T,S), write(S).

compile_structure(s(S), R) :-
nl,nl,
  write(S).