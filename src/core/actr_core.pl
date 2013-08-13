:- module(chr_actr,[fire/0,start_request/2,do_buffer_request/2,do_buffer_change/2,do_buffer_clear/1,set_production_utility/2,apply_rule/1]).
:- use_module(library(chr)).

:- use_module('scheduler.pl').
:- use_module('declarative_module.pl').
:- use_module('configuration.pl').
:- include('buffers.pl').
:- include('std_lisp.pl').

:- chr_constraint goal_focus/1, output/1, do_output/1.

goal_focus(Chunk) <=>
  declarative_module:module_request(goal,chunk(Chunk,_,_),[],ResChunk,ResState,_),
  add_chunk(ResChunk),
  set_buffer(goal,ResChunk),
  set_buffer_state(goal,ResState).
    
%now(Now) \ output(X) <=> Time is Now + 1, add_q(Time,do_output(X)).
%do_output(X) <=> write(X),nl.
output(X) <=> write(output:X),nl.  % reference p. 166: eval output, bind calls occur during fire event => immediately

% TODO
:- chr_constraint get_context/1, context/1, collect_context/1.

chunk_has_slot(_,_,V), get_context(_) ==> context([V]).
get_context(Context) <=> collect_context(Context). % no more slots => collect results

context([nil]) <=> true.
context(C1),context(C2) <=> append(C1,C2,C), context(C).
context(C), collect_context(Context) <=> Context = C.  

:- chr_constraint set_production_utility/2, production_utility/2, set_default_utilities/1, conflict_set/1, choose/0, apply_rule/1, reward/2, set_reward/2, trigger_reward/1, to_reward/2.

set_production_utility(P,U), production_utility(P,_) <=> 
  production_utility(P,U).
  
set_production_utility(P,U) <=> 
  production_utility(P,U).
  
set_default_utilities([]) <=> true.
set_default_utilities([P|Ps]) <=>
  set_production_utility(P,5),
  set_default_utilities(Ps).

conflict_set(_) \ conflict_set([]) <=> true.
  
find-max-utility @ production_utility(P1,U1), production_utility(P2,U2) \ conflict_set(P1), conflict_set(P2) <=>
  U1 >= U2 |
  conflict_set(P1).

choose, conflict_set([]) <=>
  no_rule.
  
choose @ choose, conflict_set(P) <=>
  P \== [] |
  get_now(Now),
  Time is Now + 0.05,
  write('going to apply rule '),write(P),nl,
  add_q(Time,0,apply_rule(P)).
  
apply_rule(P) ==> P \== [] | write('firing rule '),write(P),nl.

apply_rule(P) ==> P \== [] | get_now(Now), to_reward(P,Now).

apply_rule(P), reward(P,R) ==>
  P \== [] |
  trigger_reward(R),
  write('reward triggered by rule '),
  write(P),nl.
  
set_reward(P,R), reward(P,_) <=>
  reward(P,R).
  
set_reward(P,R) <=>
  reward(P,R).
  
trigger_reward(R), production_utility(P,U) \ to_reward(P,FireTime) <=>
  calc_reward(R,FireTime,Reward),
  get_conf(alpha,Alpha),
  NewU is U + Alpha*(Reward-U),
  set_production_utility(P,NewU),
  write('triggered reward for rule: '),write(P),nl.
  
calc_reward(R,FireTime,Reward) :-
  get_now(Now),
  Reward is R - (Now-FireTime).
  
trigger_reward(_) <=> true. % no more rules to reward
