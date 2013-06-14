:- include('actr_core.pl').
:- chr_constraint run/0, fire/0.


start@buffer(goal,_,A),chunk(A,count-from),chunk_has_slot(A,start,B),chunk_has_slot(A,count,nil)\fire<=>B\==nil|buffer_change(goal,chunk(_,_,[ (count,B)])),buffer_request(retrieval,chunk(_,count-order,[ (first,B)])),conflict_resolution.
increment@buffer(goal,_,A),chunk(A,count-from),chunk_has_slot(A,count,C),chunk_has_slot(A,end,D),buffer(retrieval,_,B),chunk(B,count-order),chunk_has_slot(B,first,C),chunk_has_slot(B,second,E)\fire<=>C\==nil,D\==C,E\==nil|buffer_change(goal,chunk(_,_,[ (count,E)])),buffer_request(retrieval,chunk(_,count-order,[ (first,E)])),output(C),conflict_resolution.
stop@buffer(goal,_,A),chunk(A,count-from),chunk_has_slot(A,count,B),chunk_has_slot(A,end,B)\fire<=>B\==nil|buffer_clear(goal),output(B),conflict_resolution.
init@run<=>true|
  add_buffer(retrieval,declarative_module),add_buffer(goal,declarative_module),
  lisp_chunktype([count-order,first,second]),lisp_chunktype([count-from,start,end,count]),lisp_adddm([[b,isa,count-order,first,1,second,2],[c,isa,count-order,first,2,second,3],[d,isa,count-order,first,3,second,4],[e,isa,count-order,first,4,second,5],[f,isa,count-order,first,5,second,6],[first-goal,isa,count-from,start,2,end,6]]),lisp_goalfocus([first-goal]),
  now(0),fire,nextcyc.

fire <=> write('No rule matches -> Schedule next conflict resolution event'),nl,after_next_event(do_conflict_resolution).