:- use_module(library(chr)).
:- chr_constraint lisp_chunktype/1,lisp_adddm/1,lisp_goalfocus/1.

lisp_chunktype([Type|Slots]) <=>
  add_chunk_type(Type,Slots).
  
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