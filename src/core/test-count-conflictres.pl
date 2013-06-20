:- include('actr_core.pl').
:- chr_constraint run/0, fire/0.


delay-start@fire,buffer(goal,_,A),chunk(A,count-from),chunk_has_slot(A,start,B),chunk_has_slot(A,count,nil)==>B\==nil|conflict_set(start).
start@buffer(goal,_,A),chunk(A,count-from),chunk_has_slot(A,start,B),chunk_has_slot(A,count,nil)\apply_rule(start)<=>B\==nil|buffer_change(goal,chunk(_,_,[ (count,B)])),buffer_request(retrieval,chunk(_,count-order,[ (first,B)])),conflict_resolution.
delay-increment@fire,buffer(goal,_,A),chunk(A,count-from),chunk_has_slot(A,count,C),chunk_has_slot(A,end,D),buffer(retrieval,_,B),chunk(B,count-order),chunk_has_slot(B,first,C),chunk_has_slot(B,second,E)==>C\==nil,D\==C,E\==nil|conflict_set(increment).
increment@buffer(goal,_,A),chunk(A,count-from),chunk_has_slot(A,count,C),chunk_has_slot(A,end,D),buffer(retrieval,_,B),chunk(B,count-order),chunk_has_slot(B,first,C),chunk_has_slot(B,second,E)\apply_rule(increment)<=>C\==nil,D\==C,E\==nil|buffer_change(goal,chunk(_,_,[ (count,E)])),buffer_request(retrieval,chunk(_,count-order,[ (first,E)])),output(C),conflict_resolution.
delay-stop@fire,buffer(goal,_,A),chunk(A,count-from),chunk_has_slot(A,count,B),chunk_has_slot(A,end,B)==>B\==nil|conflict_set(stop).
stop@buffer(goal,_,A),chunk(A,count-from),chunk_has_slot(A,count,B),chunk_has_slot(A,end,B)\apply_rule(stop)<=>B\==nil|buffer_clear(goal),output(B),conflict_resolution.
init@run<=>true|set_default_utilities([stop,increment,start]),add_buffer(retrieval,declarative_module),add_buffer(goal,declarative_module),
lisp_chunktype([count-order,first,second]),
lisp_chunktype([count-from,start,end,count]),
lisp_chunktype([chunk]),
lisp_adddm([[b,isa,count-order,first,1,second,2],[c,isa,count-order,first,2,second,3],[d,isa,count-order,first,3,second,4],[e,isa,count-order,first,4,second,5],
[f,isa,count-order,first,5,second,6],
[2,isa,chunk],
[3,isa,chunk],
[4,isa,chunk],
[5,isa,chunk],
[6,isa,chunk],
[first-goal,isa,count-from,start,2,end,4]]),lisp_goalfocus([first-goal]),lisp_spp([start,:,u,8]),lisp_spp([increment,:,reward,15]),now(0),conflict_resolution,nextcyc.
no-rule@fire<=>true|write('fire entfernt'),nl,conflict_set([]),choose.
