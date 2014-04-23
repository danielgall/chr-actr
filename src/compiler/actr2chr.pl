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
  process(Structure), !,
  footers,
  file(end).

  
:- chr_constraint process/1, init/1, init_rule/0, init_utilities/0, init_utilities/1, lisp_function/2, slice/2, buffer_test/3, slot_test/3, buffer_query/1, query_test/3, complete_rule/0, clear_symbol_table/0, buffer_modification/2, buffer_request/3, buffer_clearing/1, output/1, slot_value_pair/2.

process([]) <=> true.

process(model(_, Rules)) <=>
  % produce header of init rule
  init([add_buffer(retrieval,declarative_module), add_buffer(goal,declarative_module),lisp_chunktype([chunk])]),
  % compile production rules and functions
  process(Rules),
  % initialize utilities of all production rules
  init_utilities,
  % footer of init rule
  init([now(0),conflict_resolution,nextcyc]),
  % put init rule together
  init_rule.
  
process([lisp_function(Functor,Args)|Rules]) <=>
  lisp_function(Functor,Args),
  process(Rules).
  
process([production_rule(Name, LHS, RHS)|Rules]) <=>
  % initialize rule slices
  slice(name,Name),
  slice(hk,[]),
  slice(hr,[]),
  slice(guard,[]),
  slice(body,[]),
  
  %process rule parts
  process(LHS),
  process(RHS),
  
  % put rule together
  complete_rule,
  
  % process next rules
  process(Rules).
  
%
% Buffer Tests
 
process([buffer_test(Buffer,ChunkType,SlotTests)|Conditions]) <=>
  process(SlotTests),
  buffer_test(Buffer,ChunkType,_), % introduce new chunk with anonymous name _ associated to this buffer test
  process(Conditions).
  
process([slot_test(M,S,V)|SlotTests]) <=>
  slot_test(M,S,V),
  process(SlotTests).
  
%
% Buffer Queries

process([buffer_query(Buffer,QueryTests)|Conditions]) <=>
  process(QueryTests),
  buffer_query(Buffer),
  process(Conditions).
  
process([query_test(M,I,V)|QueryTests]) <=>
  query_test(M,I,V),
  process(QueryTests).
  
%
% Buffer Actions  

process([buffer_modification(Buffer,SVPs)|Actions]) <=> 
  process(SVPs),
  buffer_modification(Buffer, []),
  process(Actions).
  
process([buffer_request(Buffer,ChunkType,SVPs)|Actions]) <=> 
  process(SVPs),
  buffer_request(Buffer,ChunkType,[]),
  process(Actions).
  
process([buffer_clearing(Buffer)|Actions]) <=> 
  buffer_clearing(Buffer),
  process(Actions).
  
process([output(O)|Actions]) <=> 
  output(O),
  process(Actions).
  
process([slot_value_pair(S,V)|SVPs]) <=>
  slot_value_pair(S,V),
  process(SVPs).
  
%%%%%%%%%%%%% Lisp Functions %%%%%%%%%%%%%%%

lisp_function(Functor,Args) <=>
  atom_chars(Functor,Chars),
  delete(Chars,-,CharsR),
  atom_chars(Functor1,CharsR),
  atom_concat(lisp_,Functor1,ResFunctor),
  I =.. [ResFunctor|[Args]],
  init([I]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%% PRODUCTION RULES %%%%%%%%%%%%%

%%%%%%%%%%%% Buffer Tests %%%%%%%%%%%%%%%%%%%

%% duplicate slot tests

slice(hk,[chunk_has_slot(C,S,V1)]) \ slice(hk,[chunk_has_slot(C,S,V2)]) <=>
  V1=V2.
  
%%%% collect slot_tests for a buffer_test %%%

%%% constant values
%% unmodified slot test with constant value
buffer_test(_,_,C) \ slot_test(=,S,val(V)) <=>
  slice(hk,[chunk_has_slot(C,S,V)]).
  
%% modified slot test with constant value
buffer_test(_,_,C) \ slot_test(M,S,val(V)) <=>
  slice(hk,[chunk_has_slot(C,S,X)]),
  return_modifier(M,X,V,Res),
  slice(guard,Res).

%%% variables
%% unmodified slot test with variable
% variable already in symbol table
symbol_table(V,Var), buffer_test(_,_,C) \ slot_test(=,S,var(V)) <=>
  slice(hk,[chunk_has_slot(C,S,Var)]).
  
% variable not yet in symbol table
buffer_test(_,_,C) \ slot_test(=,S,var(V)) <=>
  symbol_table(V,Var), % remember variable assignment
  slice(hk,[chunk_has_slot(C,S,Var)]),
  slice(guard,[Var \== nil]).

%% modified slot test with variable
% variable already in symbol table
symbol_table(V,Var), buffer_test(_,_,C) \ slot_test(M,S,var(V)) <=>
  slice(hk,[chunk_has_slot(C,S,X)]),
  return_modifier(M,X,Var,Res),
  slice(guard,Res).
  
% variable not yet in symbol table
buffer_test(_,_,C) \ slot_test(M,S,var(V)) <=>
  symbol_table(V,Var), % remember variable assignment
  slice(hk,[chunk_has_slot(C,S,X)]),
  return_modifier(M,X,Var,Res),
  slice(guard,Res). 
  
%%% complete this buffer test
buffer_test(Buffer,ChunkType,C) <=>
  slice(hk,[chunk(C,ChunkType)]),
  slice(hk,[buffer(Buffer,_,C)]).

%%% translate ACT-R slot modifiers to Prolog comparators  
return_modifier(-,X,Y,[X \== Y]).
return_modifier(<,X,Y,[X < Y]).  
return_modifier(>,X,Y,[X > Y]).
% TODO add <= >=  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%% Buffer Query %%%%%%%%%%%%%%%%%%%%

%% collect query_tests for a buffer_query %%

%%% unmodified
%% unmodified with constant value
buffer_query(Buffer) \ query_test(=,I,val(V)) <=>
  slice(hk,[buffer_state(Buffer,I,V)]).
  
%% unmodified with variable which exists in symbol table  
buffer_query(Buffer), symbol_table(V,Var) \ query_test(=,I,var(V)) <=>
  slice(hk,[buffer_state(Buffer,I,Var)]).
  
%% unmodified with variable which does not exist yet
buffer_query(Buffer) \ query_test(=,I,var(V)) <=>
  symbol_table(V,Var),
  slice(hk,[buffer_state(Buffer,I,Var)]).
  
%%% modified
%% modified with constant value
buffer_query(Buffer) \ query_test(M,I,val(V)) <=>
  slice(hk,[buffer_state(Buffer,I,X)]),
  return_modifier(M,X,V,Res),
  slice(guard,Res).
  
%% modified with variable which exists in symbol table  
buffer_query(Buffer), symbol_table(V,Var) \ query_test(M,I,var(V)) <=>
  slice(hk,[buffer_state(Buffer,I,X)]),
  return_modifier(M,X,Var,Res),
  slice(guard,Res).
  
%% modified with variable which does not exist yet
buffer_query(Buffer) \ query_test(M,I,var(V)) <=>
  symbol_table(V,Var),
  slice(hk,[buffer_state(Buffer,I,X)]),
  return_modifier(M,X,Var,Res),
  slice(guard,Res).
    
buffer_query(_) <=> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%% Buffer Modification %%%%%%%%%%%%%

buffer_modification(Buffer,SVPs), slot_value_pair(S,val(V)) <=>
  buffer_modification(Buffer,[(S,V)|SVPs]).

symbol_table(V,Var) \ buffer_modification(Buffer,SVPs), slot_value_pair(S,var(V)) <=>
  buffer_modification(Buffer,[(S,Var)|SVPs]).
  
buffer_modification(_,_), slot_value_pair(_,var(_)) <=>
  fail. % vars on rhs must be bound, i.e. occur on lhs

buffer_modification(Buffer,SVPs) <=>
  slice(body,[buffer_change(Buffer,chunk(_,_,SVPs))]).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%% Buffer Request %%%%%%%%%%%%%

buffer_request(Buffer,ChunkType,SVPs), slot_value_pair(S,val(V)) <=>
  buffer_request(Buffer,ChunkType,[(S,V)|SVPs]).
  
symbol_table(V,Var) \ buffer_request(Buffer,ChunkType,SVPs), slot_value_pair(S,var(V)) <=>
  buffer_request(Buffer,ChunkType,[(S,Var)|SVPs]).
  
buffer_request(_,_,_), slot_value_pair(_,var(_)) <=>
  fail. % vars on rhs must be bound, i.e. occur on lhs

buffer_request(Buffer,ChunkType,SVPs) <=>
  slice(body,[buffer_request(Buffer,chunk(_,ChunkType,SVPs))]).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%% Buffer Clear %%%%%%%%%%%%%%%%%%%%

buffer_clearing(Buffer) <=>
  slice(body,[buffer_clear(Buffer)]).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%% Output %%%%%%%%%%%%%%%%%%%%

output(val(V)) <=>
  slice(body,[output(V)]).
  
symbol_table(V,Var) \ output(var(V)) <=>
  slice(body,[output(Var)]).

output(var(_)) <=>
  fail. % vars on rhs must be bound, i.e. occur on lhs
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%% Rule Composition %%%%%%%%%%%%%

% collect slices
slice(T,L1), slice(T,L2) <=>
  append(L1,L2,L),
  slice(T,L).
  
% complete rule
complete_rule, slice(name, Name), slice(hk,Hk), slice(hr,Hr), slice(guard,Guard), slice(body,Body) <=>
  chrl(delay-Name,[fire|Hk],Hr,Guard,[conflict_set(rule(Name,[fire|Hk],Hr))]),
  append(Body,[conflict_resolution],ResBody),
  chrl(Name,Hk,[apply_rule(rule(Name,[fire|Hk],Hr))|Hr],Guard,ResBody),
  init_utilities([Name]),
  clear_symbol_table.
  
clear_symbol_table \ symbol_table(_,_) <=> true.
clear_symbol_table <=> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%% Init Rule %%%%%%%%%%%%%%%%%%

init(L1), init(L2) <=> 
  append(L2,L1,L),
  init(L).
  
init_rule, init(Body) <=>
  chrl(init,[],[run],[],Body).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%% Init Utilities %%%%%%%%%%%%%%%

init_utilities(L1), init_utilities(L2) <=>
  append(L1,L2,L),
  init_utilities(L).
  
init_utilities, init_utilities(L) <=>
  init([set_default_utilities(L)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

footers :-
  chrl(no-rule,[],[fire],[],[conflict_set([]),choose]).
   

%%%%%%%%%%%%%%
%%% Writer %%%
%%%%%%%%%%%%%%
   
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
  write(S, 'file_search_path(chractr,\'.\').\n'),
  write(S, ':- include(chractr(\'actr_core.pl\')).\n'),
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

