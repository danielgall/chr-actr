%
% Production Utilities
%

:- chr_constraint set_production_utility/2, production_utility/2, set_default_utilities/1, conflict_set/1, choose/0, apply_rule/1, reward/2, set_reward/2, trigger_reward/1, to_reward/2.

set_production_utility(P,U), production_utility(P,_) <=> 
  production_utility(P,U).
  
set_production_utility(P,U) <=> 
  production_utility(P,U).
  
set_default_utilities([]) <=> true.
set_default_utilities([P|Ps]) <=>
  set_production_utility(P,5),
  set_default_utilities(Ps).

  
%
% Refraction
%

:- chr_constraint instantiation/1.
% holds all instantiations of a rule

:- chr_constraint check_changed/3.

instantiation(R) ==> writeln(instantiation(R)).

% prevent rule from firing if it already has been instantiated with the same values
instantiation(R) \ conflict_set(R) <=> true.

% history cleanup:
% Rules can fire on the same instantition again, if parts of the instantiation have been removed
% an recreated again.


buffer(B,_,C1) \ instantiation(rule(_,Hk,_)) <=> member(buffer(B,_,C2),Hk), C1 \== C2 | true.
chunk_has_slot(C,S,V1) \ instantiation(rule(_,Hk,_)) <=> member(chunk_has_slot(C,S,V2),Hk), V1 \== V2 | writeln(removing), true.

/*
% check if buffer has changed for all buffers
fire, 
buffer(B,_,C), instantiation(rule(R,Hk,_)) ==> member(buffer(B,_,C),Hk) | check_changed(R,C,Hk).

% check if chunk in buffer has changed
chunk_has_slot(C,S,V) \ check_changed(R,C,Hk), instantiation(rule(R,Hk,_)) <=> member(chunk_has_slot(C,S,V1),Hk), V \= V1 | true. % buffer changed at critical point -> remove instantiation
  */
%
% Conflict Resolution
% Calculate conflict set and choose rule to apply.
%

conflict_set(_) \ conflict_set([]) <=> true.
  
find-max-utility @ production_utility(P1,U1), production_utility(P2,U2), conflict_set(rule(P1,_,_)) \ conflict_set(rule(P2,_,_)) <=>
  U1 >= U2 |
  true.

choose, conflict_set([]) <=>
  no_rule.
  
choose @ choose, conflict_set(P) <=>
  P \== [] |
  get_now(Now),
  Time is Now + 0.05,
  write('going to apply rule '),write(P),nl,
  add_q(Time,0,apply_rule(P)).
  
%
% Rule Application
% Messages and reward triggering
%
  
% save instantiation for refraction
apply_rule(R) ==> instantiation(R).  
  
apply_rule(rule(P,_,_)) ==> P \== [] | write('firing rule '),write(P),nl.

apply_rule(rule(P,_,_)) ==> P \== [] | get_now(Now), to_reward(P,Now).

apply_rule(rule(P,_,_)), reward(P,R) ==>
  P \== [] |
  trigger_reward(R),
  write('reward triggered by rule '),
  write(P),nl.
  
%
% Handle rewards
%  
  
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
