%:- use_module(library(chr)).

s(s(P)) --> production_rule(P).
production_rule(production_rule(LHS,RHS)) --> lhs(LHS), [==>], rhs(RHS).

lhs(lhs(BufTests)) --> buffer_tests(BufTests).

rhs(rhs(b)) --> [b].

buffer_tests(buffer_tests(BufferTest)) --> buffer_test(BufferTest).
buffer_tests(buffer_tests(BufferTest, Next)) --> buffer_test(BufferTest), buffer_tests(Next).

buffer_test(buffer_test(Buffer, SlotTests)) --> [=], buffer(Buffer), [>], slot_tests(SlotTests).

slot_tests(slot_tests(SlotTest)) --> slot_test(SlotTest).
slot_tests(slot_tests(SlotTest, Next)) --> slot_test(SlotTest), slot_tests(Next).

slot_test(slot_test(SlotTest)) --> slot_test_var(SlotTest).
slot_test(slot_test(SlotTest)) --> slot_test_val(SlotTest).

slot_test_var(slot_test_var(Slot, Variable)) --> slot(Slot), [=], variable(Variable).
slot_test_val(slot_test_val(Slot,Value)) --> slot(Slot), value(Value).


buffer(buffer(BufferName)) --> [BufferName], { identifier(BufferName) }.
slot(slot(SlotName)) --> [SlotName], { identifier(SlotName) }.
value(value(ValueToken)) --> [ValueToken], {identifier(ValueToken)}.
variable(variable(VariableName)) --> [VariableName], {identifier(VariableName)}.

identifier(X) :-
  X \== '=', X \== '>', X \== '+'.
  
parse(Tokens, Structure) :-
  s(Structure, Tokens, []).
