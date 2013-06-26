:- use_module(library(chr)).
:- chr_constraint lisp_chunktype/1,lisp_adddm/1,lisp_goalfocus/1,lisp_spp/1,lisp_sgp/1.

lisp_chunktype([Type|Slots]) <=>
  add_chunk_type(Type,Slots),
  declarative_module:add_chunk_type(Type,Slots).
  
lisp_adddm([]) <=> true.
  
lisp_adddm([C|Cs]) <=>
  gen_chunk(C,Chunk),
  add_dm(Chunk),
  lisp_adddm(Cs).
  
gen_chunk([Name,isa,Type|Slots],Res) :-
  gen_slot_value_pairs(Slots,ResSlots),
  Res = chunk(Name,Type,ResSlots).
  
gen_slot_value_pairs([],[]).
gen_slot_value_pairs([S,V|Slots],[(S,V)|Res]) :-
  gen_slot_value_pairs(Slots,Res).
  
lisp_goalfocus([Chunk]) <=>
  goal_focus(Chunk).

  
lisp_sgp([]) <=>
  true.
lisp_sgp([:,Var,Val|Rest]) <=>
  set_conf(Var,Val),
  lisp_sgp(Rest).
  
lisp_spp([]) <=>
  true.  
  
lisp_spp([_]) <=>
  true.
  
lisp_spp([P,:,u,U|Rest]) <=>
  set_production_utility(P,U),
  lisp_spp([P|Rest]).
  
lisp_spp([P,:,reward,R|Rest]) <=>
  set_reward(P,R),
  lisp_spp([P|Rest]).
  