:- module(parser, [parse/2]).

s(model(ModelName,Rs)) --> ['(', 'define-model', ModelName ],!, rules(Rs), [')'].

% rules are lisp functions of the form (functor arg ...), i.e. lisp lists with a functor at first place
% Production rules are special functions with functor p
rules([R]) --> production_rule(R).
rules([R|Rs]) --> production_rule(R), rules(Rs).

rules([F]) --> lisp_function(F).
rules([F|Fs]) --> lisp_function(F), rules(Fs).

lisp_function(lisp_function(Functor,Args)) --> lisp_list(List), { [Functor|Args] = List }. % separate functor from arguments in list [functor,arg1,arg2,...]


lisp_list(L) --> ['('],!, lisp_listcontent(L), [')'],!.
lisp_listcontent([X]) --> [X], { identifier(X) }.
lisp_listcontent([X|Xs]) --> [X], {identifier(X)}, lisp_listcontent(Xs).
lisp_listcontent([L]) --> lisp_list(L).
lisp_listcontent([L|Xs]) --> lisp_list(L), lisp_listcontent(Xs).

% A production rule has a name, a left-hand-side (LHS) and a right-hand-side (RHS)
production_rule(production_rule(Name, LHS, RHS)) --> ['(', p, Name],{ identifier(Name) },!, lhs(LHS), [==>],!, rhs(RHS), [')'],!.

% LHS consists of one or more conditions
lhs([C]) --> condition(C).
lhs([C|Cs]) --> condition(C), lhs(Cs).

% A condition is either a buffer test or a query
condition(BufTest) --> buffer_test(BufTest).
condition(BufQuery) --> buffer_query(BufQuery).

% A buffer test specifies a buffer and a chunk type and is followed by an arbitrary number of slot_tests and is indicated by a =
buffer_test(buffer_test(Buffer,ChunkType,SlotTests)) --> [=, Buffer, >, isa, ChunkType],{ identifier(Buffer), identifier(ChunkType)},!, slot_tests(SlotTests).

% a slot_test consists of a (modified) slot_variable_pair or slot_value_pair
% there may be multiple slot_tests in a buffer_test
slot_tests([slot_test(M,S,var(V))|SVPs]) --> [M, S, =, V],{ slot_modifier(M), identifier(S), identifier(V) },!, slot_tests(SVPs).
slot_tests([slot_test(=,S,var(V))|SVPs]) --> [S, =, V],{ identifier(S), identifier(V) },!, slot_tests(SVPs).

slot_tests([slot_test(M,S,val(V))|SVPs]) --> [M, S, V],{ slot_modifier(M), identifier(S), identifier(V) },!, slot_tests(SVPs).
slot_tests([slot_test(=,S,val(V))|SVPs]) --> [S, V], { identifier(S), identifier(V) },!, slot_tests(SVPs).

slot_tests([]) --> [].

% A buffer_query specifies a buffer and an arbitrary number of query_tests and is indicated by a ?
buffer_query(buffer_query(Buffer,QueryTests)) --> [?, Buffer, >],{ identifier(Buffer) },!, query_tests(QueryTests).

% A query_test consists of a (modified) item-value-pair or item-variable-pair
% there may be multiple query_tests in a buffer_query
query_tests([query_test(M,I,var(V))|QTs]) --> [M, I, =, V],{ query_modifier(M), identifier(I), identifier(V) },!, query_tests(QTs).
query_tests([query_test(=,I,var(V))|QTs]) --> [I, =, V],{ identifier(I), identifier(V) },!, query_tests(QTs).

query_tests([query_test(M,I,val(V))|QTs]) --> [M, I, V],{ query_modifier(M), identifier(I), identifier(V) },!, query_tests(QTs).
query_tests([query_test(=,I,val(V))|QTs]) --> [I, V],{ identifier(I), identifier(V) },!, query_tests(QTs).

query_tests([]) --> [].


rhs([A]) --> action(A).
rhs([A|As]) --> action(A), rhs(As).

action(BufMod) --> buffer_modification(BufMod).
action(BufReq) --> buffer_request(BufReq).
action(BufCl)  --> buffer_clearing(BufCl).
action(Output) --> output(Output).

buffer_modification(buffer_modification(Buffer,SVPs)) --> [=, Buffer, >],{ identifier(Buffer) },!, slot_value_pairs(SVPs).

buffer_request(buffer_request(Buffer,ChunkType,SVPs)) --> [+, Buffer, >, isa, ChunkType],{ identifier(Buffer), identifier(ChunkType) },!, slot_value_pairs(SVPs).

buffer_clearing(buffer_clearing(Buffer)) --> [-, Buffer, >], { identifier(Buffer) },!.

output(output(var(Output))) --> [!, output, !, '(',=, Output,')'], { identifier(Output) },!.
output(output(val(Output))) --> [!, output, !, '(', Output,')'], { identifier(Output) },!.

% slot_variable_pair
slot_value_pairs([slot_value_pair(S,var(V))|SVPs]) --> [S, =, V], { identifier(S), identifier(V) },!,slot_value_pairs(SVPs).
% slot_value_pair
slot_value_pairs([slot_value_pair(S,val(V))|SVPs]) --> [S, V],{ identifier(S), identifier(V) },!, slot_value_pairs(SVPs).
slot_value_pairs([]) --> [].


% no keywords/special chars
identifier(X) :-
  X \== '=', X \== '>', X \== '+', X \== '-', X \== '!', X \== '(', X \== ')', X \== '==>'.

% list of allowed slot_modifiers
slot_modifier(M) :-
  M == '-'.

% list of allowed query_modifiers
query_modifier(M) :-
  M == '-'.
  
parse(Tokens, Structure) :-
  s(Structure, Tokens, []).