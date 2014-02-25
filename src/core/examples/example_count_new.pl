file_search_path(chractr,'.').
:- include(chractr('actr_core.pl')).
:- chr_constraint run/0, fire/0.


delay-start@fire,buffer(goal,C,A),chunk(A,count-from),chunk_has_slot(A,start,B),chunk_has_slot(A,count,nil)==>B\==nil|conflict_set(rule(start,[fire,buffer(goal,C,A),chunk(A,count-from),chunk_has_slot(A,start,B),chunk_has_slot(A,count,nil)],[])).
start@buffer(goal,C,A),chunk(A,count-from),chunk_has_slot(A,start,B),chunk_has_slot(A,count,nil)\apply_rule(rule(start,[fire,buffer(goal,C,A),chunk(A,count-from),chunk_has_slot(A,start,B),chunk_has_slot(A,count,nil)],[]))<=>B\==nil|buffer_request(retrieval,chunk(_,count-order,[ (first,B)])),buffer_change(goal,chunk(_,_,[ (count,B)])),conflict_resolution.
delay-increment@fire,buffer(retrieval,F,A),chunk(A,count-order),chunk_has_slot(A,second,D),chunk_has_slot(A,first,C),buffer(goal,G,B),chunk(B,count-from),chunk_has_slot(B,count,C),chunk_has_slot(B,end,E)==>D\==nil,C\==nil,E\==C|conflict_set(rule(increment,[fire,buffer(retrieval,F,A),chunk(A,count-order),chunk_has_slot(A,second,D),chunk_has_slot(A,first,C),buffer(goal,G,B),chunk(B,count-from),chunk_has_slot(B,count,C),chunk_has_slot(B,end,E)],[])).
increment@buffer(retrieval,F,A),chunk(A,count-order),chunk_has_slot(A,second,D),chunk_has_slot(A,first,C),buffer(goal,G,B),chunk(B,count-from),chunk_has_slot(B,count,C),chunk_has_slot(B,end,E)\apply_rule(rule(increment,[fire,buffer(retrieval,F,A),chunk(A,count-order),chunk_has_slot(A,second,D),chunk_has_slot(A,first,C),buffer(goal,G,B),chunk(B,count-from),chunk_has_slot(B,count,C),chunk_has_slot(B,end,E)],[]))<=>D\==nil,C\==nil,E\==C|output(C),buffer_request(retrieval,chunk(_,count-order,[ (first,D)])),buffer_change(goal,chunk(_,_,[ (count,D)])),conflict_resolution.
delay-stop@fire,buffer(goal,C,A),chunk(A,count-from),chunk_has_slot(A,end,B),chunk_has_slot(A,count,B)==>B\==nil|conflict_set(rule(stop,[fire,buffer(goal,C,A),chunk(A,count-from),chunk_has_slot(A,end,B),chunk_has_slot(A,count,B)],[])).
stop@buffer(goal,C,A),chunk(A,count-from),chunk_has_slot(A,end,B),chunk_has_slot(A,count,B)\apply_rule(rule(stop,[fire,buffer(goal,C,A),chunk(A,count-from),chunk_has_slot(A,end,B),chunk_has_slot(A,count,B)],[]))<=>B\==nil|output(B),buffer_clear(goal),conflict_resolution.
init@run<=>true|add_buffer(retrieval,declarative_module),add_buffer(goal,declarative_module),lisp_chunktype([chunk]),lisp_chunktype([count-order,first,second]),lisp_chunktype([count-from,start,end,count]),lisp_adddm([[b,isa,count-order,first,1,second,2],[c,isa,count-order,first,2,second,3],[d,isa,count-order,first,3,second,4],[e,isa,count-order,first,4,second,5],[f,isa,count-order,first,5,second,6],[first-goal,isa,count-from,start,2,end,4]]),lisp_goalfocus([first-goal]),
%lisp_spp([start,:,u,8]),lisp_spp([increment,:,reward,15]),
set_default_utilities([stop,increment,start]),now(0),conflict_resolution,nextcyc.
no-rule@fire<=>true|conflict_set([]),choose.
