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

buffer_change(buffer_change(Buffer, SlotChange)) --> [=], buffer(Buffer), [>], slot_change(SlotChange).
buffer_request(buffer_request(Buffer, SlotRequest)) --> [+], buffer(Buffer), [>], slot_request(SlotRequest).

slot_change(slot_change(SVP)) --> slot_variable_pair(SVP).
slot_change(slot_change(SVP)) --> slot_value_pair(SVP).

slot_request(slot_request(SVP)) --> slot_variable_pair(SVP).
slot_request(slot_request(SVP)) --> slot_value_pair(SVP).


identifier(X) :-
  X \== '=', X \== '>', X \== '+'.
  
parse(Tokens, Structure) :-
  s(Structure, Tokens, []).
