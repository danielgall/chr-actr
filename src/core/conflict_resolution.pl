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
  
apply_rule(rule(P,_,_)) ==> P \== [] | write('firing rule '),write(P),nl.

apply_rule(rule(P,_,_)) ==> P \== [] | get_now(Now), to_reward(P,Now).

apply_rule(P), reward(P,R) ==>
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
