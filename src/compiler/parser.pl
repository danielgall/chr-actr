:- module(parser, [parse/2]).
%:- use_module(library(chr)).

s(s(P)) --> production_rule(P).
production_rule(production_rule(Name, LHS, RHS)) --> ['('], [p], production_name(Name), lhs(LHS), [==>], rhs(RHS), [')'].

production_name(production_name(ProductionName)) --> [ProductionName], { identifier(ProductionName) }.

lhs(lhs(BufTests)) --> buffer_tests(BufTests).

buffer_tests(buffer_tests(BufferTest)) --> buffer_test(BufferTest).
buffer_tests(buffer_tests(BufferTest, Next)) --> buffer_test(BufferTest), buffer_tests(Next).

buffer_test(buffer_test(Buffer, SlotTests)) --> [=], buffer(Buffer), [>], slot_tests(SlotTests).

slot_tests(slot_tests(SlotTest)) --> slot_test(SlotTest).
slot_tests(slot_tests(SlotTest, Next)) --> slot_test(SlotTest), slot_tests(Next).

slot_test(slot_test(SVP)) --> slot_variable_pair(SVP).
slot_test(slot_test(SVP)) --> slot_value_pair(SVP).

slot_variable_pair(slot_variable_pair(Slot, Variable)) --> slot(Slot), [=], variable(Variable).
slot_value_pair(slot_value_pair(Slot,Value)) --> slot(Slot), value(Value).


buffer(buffer(BufferName)) --> [BufferName], { identifier(BufferName) }.
slot(slot(SlotName)) --> [SlotName], { identifier(SlotName) }.
value(value(ValueToken)) --> [ValueToken], { identifier(ValueToken) }.
variable(variable(VariableName)) --> [VariableName], { identifier(VariableName) }.

rhs(rhs(BufferOperations)) --> buffer_operations(BufferOperations).

buffer_operations(buffer_operations(BufferChange)) --> buffer_change(BufferChange).
buffer_operations(buffer_operations(BufferRequest)) --> buffer_request(BufferRequest).
buffer_operations(buffer_operations(BufferChange, Next)) --> buffer_change(BufferChange), buffer_operations(Next).
buffer_operations(buffer_operations(BufferRequest, Next)) --> buffer_request(BufferRequest), buffer_operations(Next).

buffer_change(buffer_change(Buffer, SlotRHSs)) --> [=], buffer(Buffer), [>], slot_rhss(SlotRHSs).
buffer_request(buffer_request(Buffer, SlotRHSs)) --> [+], buffer(Buffer), [>], slot_rhss(SlotRHSs).
%buffer_change(buffer_change(Buffer, SlotChanges)) --> [=], buffer(Buffer), [>], slot_changes(SlotChanges).
%buffer_request(buffer_request(Buffer, SlotRequests)) --> [+], buffer(Buffer), [>], slot_requests(SlotRequests).

slot_rhss(slot_rhss(SC)) --> slot_rhs(SC).
slot_rhss(slot_rhss(SC, Next)) --> slot_rhs(SC), slot_rhss(Next).
%slot_changes(slot_changes(SC)) --> slot_change(SC).
%slot_changes(slot_changes(SC, Next)) --> slot_change(SC), slot_changes(Next).

%slot_requests(slot_requests(SR)) --> slot_request(SR).
%slot_requests(slot_requests(SR, Next)) --> slot_request(SR), slot_requests(Next).


slot_rhs(slot_rhs(SVP)) --> slot_variable_pair(SVP).
slot_rhs(slot_rhs(SVP)) --> slot_value_pair(SVP).
%slot_change(slot_change(SVP)) --> slot_variable_pair(SVP).
%slot_change(slot_change(SVP)) --> slot_value_pair(SVP).

%slot_request(slot_request(SVP)) --> slot_variable_pair(SVP).
%slot_request(slot_request(SVP)) --> slot_value_pair(SVP).


identifier(X) :-
  X \== '=', X \== '>', X \== '+'.
  
parse(Tokens, Structure) :-
  s(Structure, Tokens, []).
