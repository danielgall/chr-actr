file_search_path(chractr,'.').
:- include(chractr('actr_core.pl')).
:- chr_constraint run/0, fire/0.


delay-train1@fire,buffer(goal,_,A),chunk(A,goal-chunk),chunk_has_slot(A,goal,training1)==>true|conflict_set(train1).
train1@buffer(goal,_,A),chunk(A,goal-chunk),chunk_has_slot(A,goal,training1)\apply_rule(train1)<=>true|buffer_change(goal,chunk(_,_,[ (goal,training2)])),buffer_request(retrieval,chunk(_,count-order,[ (first,3), (second,4)])),conflict_resolution.
delay-train2@fire,buffer(goal,_,A),chunk(A,goal-chunk),chunk_has_slot(A,goal,training2),buffer(retrieval,_,B),chunk(B,count-order),chunk_has_slot(B,first,3),chunk_has_slot(B,second,4)==>true|conflict_set(train2).
train2@buffer(goal,_,A),chunk(A,goal-chunk),chunk_has_slot(A,goal,training2),buffer(retrieval,_,B),chunk(B,count-order),chunk_has_slot(B,first,3),chunk_has_slot(B,second,4)\apply_rule(train2)<=>true|buffer_change(goal,chunk(_,goal-chunk,[ (start,2), (end,4), (goal,count)])),buffer_clear(retrieval),conflict_resolution.
delay-start@fire,buffer(goal,_,A),chunk(A,goal-chunk),chunk_has_slot(A,goal,count),chunk_has_slot(A,start,B),chunk_has_slot(A,count,nil)==>B\==nil|conflict_set(start).
start@buffer(goal,_,A),chunk(A,goal-chunk),chunk_has_slot(A,goal,count),chunk_has_slot(A,start,B),chunk_has_slot(A,count,nil)\apply_rule(start)<=>B\==nil|buffer_change(goal,chunk(_,_,[ (count,B)])),buffer_request(retrieval,chunk(_,count-order,[ (first,B)])),conflict_resolution.
delay-increment@fire,buffer(goal,_,A),chunk(A,goal-chunk),chunk_has_slot(A,goal,count),chunk_has_slot(A,count,C),chunk_has_slot(A,end,D),buffer(retrieval,_,B),chunk(B,count-order),chunk_has_slot(B,first,C),chunk_has_slot(B,second,E)==>C\==nil,D\==C,E\==nil|conflict_set(increment).
increment@buffer(goal,_,A),chunk(A,goal-chunk),chunk_has_slot(A,goal,count),chunk_has_slot(A,count,C),chunk_has_slot(A,end,D),buffer(retrieval,_,B),chunk(B,count-order),chunk_has_slot(B,first,C),chunk_has_slot(B,second,E)\apply_rule(increment)<=>C\==nil,D\==C,E\==nil|buffer_change(goal,chunk(_,_,[ (count,E)])),buffer_request(retrieval,chunk(_,count-order,[ (first,E)])),output(C),conflict_resolution.
delay-incrementx@fire,buffer(goal,_,A),chunk(A,goal-chunk),chunk_has_slot(A,goal,count),chunk_has_slot(A,count,C),chunk_has_slot(A,end,D),buffer(retrieval,_,B),chunk(B,count-order),chunk_has_slot(B,first,C),chunk_has_slot(B,second,E)==>C\==nil,D\==C,E\==nil|conflict_set(incrementx).
incrementx@buffer(goal,_,A),chunk(A,goal-chunk),chunk_has_slot(A,goal,count),chunk_has_slot(A,count,C),chunk_has_slot(A,end,D),buffer(retrieval,_,B),chunk(B,count-order),chunk_has_slot(B,first,C),chunk_has_slot(B,second,E)\apply_rule(incrementx)<=>C\==nil,D\==C,E\==nil|buffer_clear(goal),output(wrong),conflict_resolution.
delay-stop@fire,buffer(goal,_,A),chunk(A,goal-chunk),chunk_has_slot(A,goal,count),chunk_has_slot(A,count,B),chunk_has_slot(A,end,B)==>B\==nil|conflict_set(stop).
stop@buffer(goal,_,A),chunk(A,goal-chunk),chunk_has_slot(A,goal,count),chunk_has_slot(A,count,B),chunk_has_slot(A,end,B)\apply_rule(stop)<=>B\==nil|buffer_clear(goal),output(B),conflict_resolution.
init@run<=>true|now(0),set_default_utilities([stop,incrementx,increment,start,train2,train1]),add_buffer(retrieval,declarative_module),add_buffer(goal,declarative_module),lisp_chunktype([chunk]),lisp_sgp([:,esc,t]),lisp_chunktype([count-order,first,second]),lisp_chunktype([goal-chunk,goal,start,end,count]),lisp_adddm([[b,isa,count-order,first,1,second,2],[c,isa,count-order,first,2,second,3],[d,isa,count-order,first,3,second,4],[d1,isa,count-order,first,3,second,5],[e,isa,count-order,first,4,second,5],[f,isa,count-order,first,5,second,6],[first-goal,isa,goal-chunk,goal,training1]]),lisp_goalfocus([first-goal]),lisp_spp([increment,:,u,8,incrementx,:,u,0]),lisp_spp([stop,:,reward,15]),conflict_resolution,nextcyc.
no-rule@fire<=>true|conflict_set([]),choose.
