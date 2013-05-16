:- use_module(library(chr)).
:- use_module(tokenizer).
:- use_module(parser).

:- chr_constraint chr/5,chrl/5, list2goal/2, file/1, stream/2, console/0, line/1.

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
  console,
  nl,nl,
  compile_structure(Structure),
  close(S).

compile(X,S) :- getTokens(X,T), parse(T,S), write(S).

compile_structure(s(S)) :- compile_structure(S).

compile_structure(production_rule(production_name(Name), LHS, RHS)) :- 
  compile_structure(LHS, Left), 
  compile_structure(RHS, Right),
  chrl(Name, Left,[],[],Right).
  %string_concat(Name, ' @ ', S1), string_concat(S1, Left, S2), string_concat(S2, ' ==> ', S3), string_concat(S3, Right, R).
  

compile_structure(lhs(BufferTests),R) :-
  compile_structure(BufferTests,R).
  
compile_structure(buffer_tests(BufferTest),R) :- 
  compile_structure(BufferTest, R).


compile_structure(buffer_tests(BufferTest, Next),R) :- 
  compile_structure(BufferTest, RBufferTest),
  compile_structure(Next, RNext),
  append(RBufferTest,RNext,R).
  
compile_structure(buffer_test(buffer(Buffer), SlotTests),R) :- 
  %string_concat('buffer(', Buffer, S1), string_concat(S1,', Chunk1)', R).
  %R = buffer(Buffer, Chunk1).
  compile_structure(SlotTests, Chunk, R1),
  R = [buffer(Buffer,Chunk)|R1].
  
compile_structure(rhs(RHS),[]).

compile_structure(slot_tests(slot_test(slot_value_pair(slot(S),value(V)))), Chunk, R) :-
  R = [chunk_has_slot(Chunk,S,V)].
  
compile_structure(slot_tests(slot_test(slot_value_pair(slot(S),value(V))), Next), Chunk, R) :-
  compile_structure(Next, Chunk, RNext),
  R = [chunk_has_slot(Chunk,S,V)|RNext].





chrl(N,KL,RL,GL,BL) <=> 
  list2goal(KL,K),
  list2goal(RL,R),
  list2goal(GL,G),
  list2goal(BL,B),
  numbervars((K,R,G,B)), % pretty print variables
  chr(N,K,R,G,B).
  
list2goal([],G) <=> G=true.
list2goal([X],G) <=> G=X.
list2goal([X|Xs],G) <=> G=(X,Gs),
  list2goal(Xs,Gs).
  
% simplification  
console \ chr(N,K,R,G,B) <=> K == true | write(N @ R<=>G|B), write('.\n').

% propagation  
console \ chr(N,K,R,G,B) <=> R == true | write(N @ K==>G|B), write('.\n').

% simpagation
console \ chr(N,K,R,G,B) <=> K \== true, R \== true | write(N @ K\R<=>G|B), write('.\n').

file(new) <=> 
  open('out.pl', write, S),
  stream(S, write).

file(append) <=>
  open('out.pl', append, S),
  stream(S, write).
  
file(read) <=>
  open('out.pl', read, S),
  stream(S, read).
  
file(end), stream(S,write) <=> close(S).

% simplification  
stream(S, write) \ chr(N, K,R,G,B) <=> K == true | write(S, N @ R<=>G|B), write(S, '.\n').

% propagation  
stream(S, write) \ chr(N, K,R,G,B) <=> R == true | write(S, N @ K==>G|B), write(S, '.\n').

% simpagation
stream(S, write) \ chr(N, K,R,G,B) <=> K \== true, R \== true | write(S, N @ K\R<=>G|B), write(S, '.\n').

stream(S, read) <=> at_end_of_stream(S) | true.
stream(S, read) <=> \+at_end_of_stream(S) | read(S,L), line(L), stream(S,read).

line(X) <=> write(X), nl.

/*
s(
production_rule(
  production_name(name),
  lhs(
    buffer_tests(
      buffer_test(
	buffer(retrieval),
	slot_tests(
	  slot_test(
	    slot_value_pair(
	      slot(slot1),value(val1))),
	      slot_tests(
		slot_test(
		slot_value_pair(slot(slot2),value(val2)))))))),
  rhs(
    buffer_operations(
      buffer_request(
	buffer(retrieval),
	slot_request(
	  slot_value_pair(
	    slot(slot1),value(val1))))))))

  

  */