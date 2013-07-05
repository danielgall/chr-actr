:- use_module(library(chr)).
:- use_module(tokenizer).
:- use_module(parser).

:- chr_constraint chr_headers/0, chr/5,chrl/5, list2goal/2, file/1, stream/2, console/0, line/1,compile_structure/1,compile_structure2/2,compile_structure2_lhs/3,compile_structure3/3,compile_structure3_lhs/4,symbol_table/2, end_of_block/0.

readAll(S, []) :-
  at_end_of_stream(S).
  
readAll(S,[C|Cs]) :-
  \+ at_end_of_stream(S),
  get_code(S,C), readAll(S,Cs).

  
compile_file(F) :-
  open(F,read,S),
  readAll(S,Cs),
  close(S),!,
  getTokens(Cs,T),!,
  nl, nl,
  write(T),
  parse(T,Structure),!,
  nl, nl,
  write(Structure),
  console,
  nl,nl,
  chr_headers,
  compile_structure(Structure), !,
  footers,
  file(end).


compile_structure(model(ModelName,Functions)) <=> 
  compile_structure2(Functions,Inits),
  init_utilities(Utilities),
  Header = [set_default_utilities(Utilities),add_buffer(retrieval,declarative_module), add_buffer(goal,declarative_module),lisp_chunktype([chunk])],
  Footer = [now(0),conflict_resolution,nextcyc],
  append(Header,Inits,L),
  append(L,Footer,ResInits),
  chrl(init,[],[run],[],ResInits).
 
:- chr_constraint production/1, init_utilities/1. 
 
production(P), init_utilities(Utilities) <=> Utilities=[P|Rest], init_utilities(Rest).
init_utilities(Utilities) <=> Utilities=[].

compile_structure2([],Inits) <=> Inits=[].
compile_structure2([X|Xs],Res) <=>
  compile_structure2(X,Init),
  compile_structure2(Xs,Inits),
  append(Init,Inits,Res).

compile_structure2(lisp_function(FName,FArgs),Init) <=> 
  atom_chars(FName,Chars),
  delete(Chars,-,CharsR),
  atom_chars(FNameR,CharsR),
  atom_concat(lisp_,FNameR,F),
  I =.. [F|[FArgs]],
  Init = [I].
  
compile_structure2(production_rule(N,L,R),Inits) <=>
  compile_structure(production_rule(N,L,R)),
  Inits = [].
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Production rule compilation %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compile_structure(production_rule(production_name(Name), LHS, RHS)) <=> 
  compile_structure2_lhs(LHS, Head, Guard), 
  compile_structure2(RHS, ResRHS),
  end_of_block,
  append(ResRHS,[conflict_resolution],Body),
  chrl(delay-Name, [fire|Head],[],Guard,[conflict_set(Name)]),
  chrl(Name, Head,[apply_rule(Name)],Guard,Body),
  production(Name).

% LHS
%%  

compile_structure2_lhs(lhs(BufferTests),R,Guard) <=>
  compile_structure2_lhs(BufferTests,R, Guard).
  
compile_structure2_lhs(buffer_tests(BufferTest),R,Guard) <=>
  compile_structure2_lhs(BufferTest, R, Guard).


compile_structure2_lhs(buffer_tests(BufferTest, Next),R,Guard) <=> 
  compile_structure2_lhs(BufferTest, RBufferTest,Guard1),
  compile_structure2_lhs(Next, RNext,Guard2),
  append(RBufferTest,RNext,R),
  append(Guard1,Guard2,Guard).
  
compile_structure2_lhs(buffer_test(buffer(Buffer), ChunkType, SlotTests),R, Guard) <=>
  compile_structure3_lhs(SlotTests, Chunk, R1,Guard),
  R = [buffer(Buffer,_,Chunk), chunk(Chunk,ChunkType)|R1].
  
% 
% Compile slot_tests and slot_test
%

% structure: single slot_test in slot_tests-environment.
% returns: list containing that slot_test in form of a chunk_has_slot constraint
compile_structure3_lhs(slot_tests(SlotTest), Chunk, R, Guard) <=>
  compile_structure3_lhs(SlotTest, Chunk, R, Guard).

% structure: slot_test followed by next slot_tests in slot_tests-environment
% returns: list containing all the slot_tests in form of chunk_has_slot constraints
compile_structure3_lhs(slot_tests(SlotTest, Next), Chunk, R, Guard) <=>
  compile_structure3_lhs(SlotTest, Chunk, RSlotTest, Guard1),
  compile_structure3_lhs(Next, Chunk, RNext, Guard2),
  append(RSlotTest,RNext,R),
  append(Guard1,Guard2,Guard).

% structure: slot_test containing one slot-value-pair with slot S and value V
% returns: a list containing this one slot_test in form of a chunk_has_slot constraint  
compile_structure3_lhs(slot_test(slot_value_pair(slot(S),value(V))), Chunk, R,Guard) <=>
  R = [chunk_has_slot(Chunk,S,V)],
  Guard = [].

% structure: slot_test containing one slot-variable-pair with slot S and variable V
% If the variable V already has been used in this rule, it will be bound to that former variable.
% returns: a list containing this one slot_test in form of a chunk_has_slot constraint  
symbol_table(V, Var) \ compile_structure3_lhs(slot_test(slot_variable_pair(slot(S),variable(V))), Chunk, R, Guard) <=>
  R = [chunk_has_slot(Chunk,S,Var)],
  Guard = [].
  
compile_structure3_lhs(slot_test(slot_variable_pair(slot(S),variable(V))), Chunk, R, Guard) <=>
  symbol_table(V, Var),
  R = [chunk_has_slot(Chunk,S,Var)],
  Guard = [Var \== nil].

% negated slot tests
%
  
% structure: neg_slot_test containing one slot-value-pair with slot S and value V
% returns: a list containing this one slot_test in form of a chunk_has_slot constraint  
compile_structure3_lhs(neg_slot_test(slot_value_pair(slot(S),value(V))), Chunk, R,Guard) <=>
  R = [chunk_has_slot(Chunk,S,X)],
  Guard = [X \== V].

% structure: slot_test containing one slot-variable-pair with slot S and variable V
% If the variable V already has been used in this rule, it will be bound to that former variable.
% returns: a list containing this one slot_test in form of a chunk_has_slot constraint  
symbol_table(V, Var) \ compile_structure3_lhs(neg_slot_test(slot_variable_pair(slot(S),variable(V))), Chunk, R, Guard) <=>
  R = [chunk_has_slot(Chunk,S,X)],
  Guard = [X \== Var].
  
compile_structure3_lhs(neg_slot_test(slot_variable_pair(slot(S),variable(V))), Chunk, R, Guard) <=>
  symbol_table(V, Var),
  R = [chunk_has_slot(Chunk,S,X)],
  Guard = [X \== Var].
  
  
compile_structure2_lhs(buffer_query(buffer(_), queries([])),R, Guard) <=> R=[],Guard=[].
compile_structure2_lhs(buffer_query(buffer(Buffer), queries([(QueriedItem,QueryValue)|Qs])),R, Guard) <=> 
  compile_structure2_lhs(buffer_query(buffer(Buffer), queries(Qs)),RNext, _),
  append([buffer_state(Buffer,QueriedItem,QueryValue)],RNext,R),
  Guard = [].

%TODO add support for query values as bound variables  
  
% RHS
%%    
  
compile_structure2(rhs(RHS),R) <=> 
  compile_structure2(RHS,R).
  
compile_structure2(buffer_operations(BufOp),R) <=> 
  compile_structure2(BufOp,R).
  
compile_structure2(buffer_operations(BufOp, Next),R) <=> 
  compile_structure2(BufOp,RBufOp),
  compile_structure2(Next, RNext),
  append(RBufOp,RNext,R).
  
compile_structure2(buffer_request(buffer(Buffer), ChunkType, SlotRequests),R) <=>
  compile_structure3(SlotRequests,RSlots,RFuncCalls),
  append([buffer_request(Buffer, chunk(_,ChunkType,RSlots))],RFuncCalls,R).
  
compile_structure2(buffer_change(buffer(Buffer), ChunkType, SlotChanges),R) <=>
  compile_structure3(SlotChanges,RSlots,RFuncCalls),
  append([buffer_change(Buffer, chunk(_,ChunkType,RSlots))],RFuncCalls,R).
  
  
 
 
 
compile_structure2(buffer_clear(buffer(Buffer), FuncCalls),R) <=>
  compile_structure2(FuncCalls,RFuncCalls),
  list2goal(RFuncCalls, FuncCallGoals),
  R = [buffer_clear(Buffer), FuncCallGoals]. % TODO funccalls!!
  
 
compile_structure2(function_calls(FuncCalls), R) <=>
  compile_structure2(FuncCalls, R).
  
compile_structure2(function_calls(FuncCalls, Next), R) <=>
  compile_structure2(FuncCalls, RFC),
  compile_structure2(Next, RNext),
  append(RFC,RNext,R).
  
  
compile_structure2(function_call(functor(Functor), Args), R) <=>
  compile_structure2(Args, RArgs),
  F =.. [Functor|RArgs],
  R = [F].

  
compile_structure2(func_args(func_arg(value(Arg))), R) <=>
  R = [Arg].
  
compile_structure2(func_args(func_arg(value(Arg)), Next), R) <=>
  compile_structure2(Next, RNext),
  append([Arg],RNext,R).
  
  
symbol_table(V, Arg) \ compile_structure2(func_args(func_arg(variable(V))), R) <=>
  R = [Arg].
  
symbol_table(V, Arg) \ compile_structure2(func_args(func_arg(variable(V)), Next), R) <=>
  compile_structure2(Next, RNext),
  append([Arg],RNext,R).
  
compile_structure2(func_args(func_arg(variable(V))), R) <=>
  symbol_table(V, Arg),
  R = [Arg].
  
compile_structure2(func_args(func_arg(variable(V)), Next), R) <=>
  symbol_table(V, Arg),
  compile_structure2(Next, RNext),
  append([Arg],RNext,R).
  
  
compile_structure3(slot_rhss(slot_rhs(SR)), R, RFC) <=>
  RFC = [],
  compile_structure2(slot_rhs(SR), R).
  
compile_structure3(slot_rhss(slot_rhs(SR), Next), R, RNextFunctions) <=>
  compile_structure2(slot_rhs(SR), RSR),
  compile_structure3(Next, RNext,RNextFunctions),
  append(RSR,RNext,R).
  
compile_structure3(slot_rhss(function_call(F,Args)), RS, R) <=>
  RS = [],
  compile_structure2(function_call(F,Args), R).
  
compile_structure3(slot_rhss(function_call(F,Args), Next), RNextSlots, R) <=>
  compile_structure2(function_call(F,Args), RFC),
  compile_structure3(Next, RNextSlots, RNextFunctions),
  append(RFC,RNextFunctions,R).

compile_structure2(slot_rhs(slot_value_pair(slot(S),value(V))),R) <=>
  R = [(S,V)].
  
symbol_table(V, Var) \ compile_structure2(slot_rhs(slot_variable_pair(slot(S),variable(V))),R) <=>
  R = [(S,Var)].
  
compile_structure2(slot_rhs(slot_variable_pair(slot(S),variable(V))),R) <=>
  symbol_table(V, Var),
  R = [(S,Var)].
   

footers :-
  chrl(no-rule,[],[fire],[],[conflict_set([]),choose]).
   
  
%
% at end of block (which means at the end of the definition of this ACT-R production rule): 
% delete all symbol_table constraints to clear symbol table.
end_of_block \ symbol_table(_,_) <=> true.
end_of_block <=> true.

% translate chrl constraints to chr constraints containing goals instead of lists
chrl(N,KL,RL,GL,BL) <=> 
  list2goal(KL,K),
  list2goal(RL,R),
  list2goal(GL,G),
  list2goal(BL,B),
  numbervars((K,R,G,B),0,_,[singletons(true)]), % pretty print variables
  chr(N,K,R,G,B).

% takes a list and transforms it to a goal
list2goal([],G) <=> G=true.
list2goal([X],G) <=> G=X.
list2goal([X|Xs],G) <=> G=(X,Gs),
  list2goal(Xs,Gs).

%
% Output: transform chr constraints to textual CHR-rules.
% If constraint console is in store: Output on console,
% If constraint file is in store: Output to file.
%

% Output to some stream (eg. a file)

% simplification  
stream(S, write) \ chr(N, K,R,G,B) <=> K == true | write(S, N @ R<=>G|B), write(S, '.\n').

% propagation  
stream(S, write) \ chr(N, K,R,G,B) <=> R == true | write(S, N @ K==>G|B), write(S, '.\n').

% simpagation
stream(S, write) \ chr(N, K,R,G,B) <=> K \== true, R \== true | write(S, N @ K\R<=>G|B), write(S, '.\n').


%headers
stream(S, write) \ chr_headers <=>
  write(S, ':- include(\'actr_core.pl\').\n'),
  write(S, ':- chr_constraint run/0, fire/0.\n'),
  write(S, '\n\n').

%
% File I/O handling
%

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

console <=>
  stream(user_output, write).

%
%% File Input (not used)
%%

%stream(S, read) <=> at_end_of_stream(S) | true.
%stream(S, read) <=> \+at_end_of_stream(S) | read(S,L), line(L), stream(S,read).

%line(X) <=> write(X), nl.

