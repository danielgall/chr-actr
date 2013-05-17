:- use_module(library(chr)).
:- use_module(tokenizer).
:- use_module(parser).

:- chr_constraint chr/5,chrl/5, list2goal/2, file/1, stream/2, console/0, line/1,compile_structure/1,compile_structure2/2,compile_structure3/3,symbol_table/2, end_of_block/0.

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

compile_structure(s(S)) <=> compile_structure(S).

compile_structure(production_rule(production_name(Name), LHS, RHS)) <=> 
  compile_structure2(LHS, Left), 
  compile_structure2(RHS, Right),
  end_of_block,
  chrl(Name, Left,[],[],Right).

compile_structure2(lhs(BufferTests),R) <=>
  compile_structure2(BufferTests,R).
  
compile_structure2(buffer_tests(BufferTest),R) <=>
  compile_structure2(BufferTest, R).


compile_structure2(buffer_tests(BufferTest, Next),R) <=> 
  compile_structure2(BufferTest, RBufferTest),
  compile_structure2(Next, RNext),
  append(RBufferTest,RNext,R).
  
compile_structure2(buffer_test(buffer(Buffer), SlotTests),R) <=>
  compile_structure3(SlotTests, Chunk, R1),
  R = [buffer(Buffer,Chunk)|R1].
  
compile_structure2(rhs(RHS),R) <=> 
  compile_structure2(RHS,R).
  
compile_structure2(buffer_operations(BufOp),R) <=> 
  compile_structure2(BufOp,R).
  
compile_structure2(buffer_operations(BufOp, Next),R) <=> 
  compile_structure2(BufOp,RBufOp),
  compile_structure2(Next, RNext),
  append(RBufOp,RNext,R).
  
compile_structure2(buffer_request(buffer(Buffer), SlotRequests),R) <=>
  compile_structure2(SlotRequests,RSlots),
  R = [buffer_request(Buffer, chunk(_,_,RSlots))].
  
compile_structure2(buffer_change(buffer(Buffer), SlotChanges),R) <=>
  compile_structure2(SlotChanges,RSlots),
  R = [buffer_change(Buffer, chunk(_,_,RSlots))].
  
compile_structure2(slot_rhss(SR), R) <=>
  compile_structure2(SR, R).
  
compile_structure2(slot_rhss(SR, Next), R) <=>
  compile_structure2(SR, RSR),
  compile_structure2(Next, RNext),
  append(RSR,RNext,R).

compile_structure2(slot_rhs(slot_value_pair(slot(S),value(V))),R) <=>
  R = [(S,V)].
  
symbol_table(V, Var) \ compile_structure2(slot_rhs(slot_variable_pair(slot(S),variable(V))),R) <=>
  R = [(S,Var)].
  
compile_structure2(slot_rhs(slot_variable_pair(slot(S),variable(V))),R) <=>
  symbol_table(V, Var),
  R = [(S,Var)].
   
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

	    
name@buffer(goal,A),chunk_has_slot(A,isa,xnedder),chunk_has_slot(A,slot,B),buffer(retrieval,C),chunk_has_slot(C,slot1,val1),chunk_has_slot(C,slot2,B)==>
true|buffer_request(retrieval,chunk(D,E,[ (slot1,B), (slot2,test)| (slot3,F)])).

  

  */

compile_structure3(slot_tests(SlotTest), Chunk, R) <=>
  compile_structure3(SlotTest, Chunk, R).
  
compile_structure3(slot_tests(SlotTest, Next), Chunk, R) <=>
  compile_structure3(SlotTest, Chunk, RSlotTest),
  compile_structure3(Next, Chunk, RNext),
  append(RSlotTest,RNext,R).

compile_structure3(slot_test(slot_value_pair(slot(S),value(V))), Chunk, R) <=>
  R = [chunk_has_slot(Chunk,S,V)].

symbol_table(V, Var) \ compile_structure3(slot_test(slot_variable_pair(slot(S),variable(V))), Chunk, R) <=>
  R = [chunk_has_slot(Chunk,S,Var)].
  
compile_structure3(slot_test(slot_variable_pair(slot(S),variable(V))), Chunk, R) <=>
  symbol_table(V, Var),
  R = [chunk_has_slot(Chunk,S,Var)].
  
end_of_block \ symbol_table(_,_) <=> true.
end_of_block <=> true.
  
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

