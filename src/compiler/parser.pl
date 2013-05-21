:- module(parser, [parse/2]).

% DCG rule implementation of grammar:
% s -> production_rule
% production_rule -> '(p' production_name lhs '==>' rhs ')'
% production_name -> // some identifier
% lhs -> buffer_tests
% buffer_tests -> buffer_test | buffer_test buffer_tests
% buffer_test -> '=' buffer '>' slot_tests
% slot_tests -> slot_test | slot_test slot_tests
% slot_test --> slot_variable_pair
% slot_test --> slot_value_pair
% slot_variable_pair -> slot '=' variable
% slot_value_pair -> slot value
% slot | variable | value -> // some identifier
% rhs -> buffer_operations
% buffer_operations -> buffer_change | buffer_request | buffer_change buffer_operations | buffer_request buffer_operations
% buffer_change -> '=' buffer '>' slot_rhss
% buffer_request -> '+' buffer '>' slot_rhss
% slot_rhss -> slot_rhs | slot_rhs slot_rhss
% slot_rhs -> slot_variable_pair | slot_value_pair
% identifier -> // everything that is not '=', '>', '+', '-' or '!' (which means: a reserved word).
%

s(s(P)) --> production_rule(P).
s(s(P, Ss)) --> production_rule(P), s(Ss).
production_rule(production_rule(Name, LHS, RHS)) --> ['('], [p], production_name(Name), lhs(LHS), [==>], rhs(RHS), [')'].

production_name(production_name(ProductionName)) --> [ProductionName], { identifier(ProductionName) }.

lhs(lhs(BufTests)) --> buffer_tests(BufTests).

buffer_tests(buffer_tests(BufferTest)) --> buffer_test(BufferTest).
buffer_tests(buffer_tests(BufferTest, Next)) --> buffer_test(BufferTest), buffer_tests(Next).

buffer_test(buffer_test(Buffer, SlotTests)) --> [=], buffer(Buffer), [>], slot_tests(SlotTests).

slot_tests(slot_tests(SlotTest)) --> slot_test(SlotTest).
slot_tests(slot_tests(SlotTest, Next)) --> slot_test(SlotTest), slot_tests(Next).

slot_tests(slot_tests(SlotTest)) --> neg_slot_test(SlotTest).
slot_tests(slot_tests(SlotTest, Next)) --> neg_slot_test(SlotTest), slot_tests(Next).

slot_test(slot_test(SVP)) --> slot_variable_pair(SVP).
slot_test(slot_test(SVP)) --> slot_value_pair(SVP).

neg_slot_test(neg_slot_test(SVP)) --> [-], slot_variable_pair(SVP).
neg_slot_test(neg_slot_test(SVP)) --> [-], slot_value_pair(SVP).

slot_variable_pair(slot_variable_pair(Slot, Variable)) --> slot(Slot), [=], variable(Variable).
slot_value_pair(slot_value_pair(Slot,Value)) --> slot(Slot), value(Value).

buffer(buffer(BufferName)) --> [BufferName], { identifier(BufferName) }.
slot(slot(SlotName)) --> [SlotName], { identifier(SlotName) }.
value(value(ValueToken)) --> [ValueToken], { identifier(ValueToken) }.
variable(variable(VariableName)) --> [VariableName], { identifier(VariableName) }.

rhs(rhs(BufferOperations)) --> buffer_operations(BufferOperations).

buffer_operations(buffer_operations(BufferChange)) --> buffer_change(BufferChange).
buffer_operations(buffer_operations(BufferRequest)) --> buffer_request(BufferRequest).
buffer_operations(buffer_operations(BufferClear)) --> buffer_clear(BufferClear).
buffer_operations(buffer_operations(BufferChange, Next)) --> buffer_change(BufferChange), buffer_operations(Next).
buffer_operations(buffer_operations(BufferRequest, Next)) --> buffer_request(BufferRequest), buffer_operations(Next).
buffer_operations(buffer_operations(BufferClear, Next)) --> buffer_request(BufferClear), buffer_operations(Next).

buffer_change(buffer_change(Buffer, SlotRHSs)) --> [=], buffer(Buffer), [>], slot_rhss(SlotRHSs).
buffer_request(buffer_request(Buffer, SlotRHSs)) --> [+], buffer(Buffer), [>], slot_rhss(SlotRHSs).
buffer_clear(buffer_clear(Buffer)) --> [-], buffer(Buffer), [>].
buffer_clear(buffer_clear(Buffer, FunctionCalls)) --> [-], buffer(Buffer), [>], function_calls(FunctionCalls).

slot_rhss(slot_rhss(SC)) --> slot_rhs(SC).
slot_rhss(slot_rhss(SC, Next)) --> slot_rhs(SC), slot_rhss(Next).

slot_rhs(slot_rhs(SVP)) --> slot_variable_pair(SVP).
slot_rhs(slot_rhs(SVP)) --> slot_value_pair(SVP).
slot_rhs(slot_rhs(Call)) --> function_call(Call).

function_calls(function_calls(FunctionCall)) --> function_call(FunctionCall).
function_calls(function_calls(FunctionCall, Next)) --> function_call(FunctionCall), function_calls(Next).

function_call(function_call(Functor, Args)) --> sfunctor(Functor), ['('], func_args(Args), [')'].
sfunctor(functor(Functor)) -->  [!, Functor, !], { identifier(Functor) }.
func_args(func_args(Arg)) --> func_arg(Arg).
func_args(func_args(Arg, Next)) --> func_arg(Arg), func_args(Next).
func_arg(func_arg(Arg)) --> [=], variable(Arg).
func_arg(func_arg(Arg)) --> value(Arg).



identifier(X) :-
  X \== '=', X \== '>', X \== '+', X \== '-', X \== '!', X \== '(', X \== ')'.
  
parse(Tokens, Structure) :-
  s(Structure, Tokens, []).
