:- include('actr_core.pl').
:- chr_constraint run/0, fire/0.


delay-train1@fire,buffer(goal,_,A),chunk(A,goal-chunk),chunk_has_slot(A,goal,training)==>true|conflict_set(train1).
train1@buffer(goal,_,A),chunk(A,goal-chunk),chunk_has_slot(A,goal,training)\apply_rule(train1)<=>true|buffer_request(retrieval,chunk(_,count-order,[ (first,2), (second,3)])),conflict_resolution.
delay-train2@fire,buffer(goal,_,A),chunk(A,goal-chunk),chunk_has_slot(A,goal,training),buffer(retrieval,_,B),chunk(B,count-order),chunk_has_slot(B,first,2),chunk_has_slot(B,second,3)==>true|conflict_set(train2).
train2@buffer(goal,_,A),chunk(A,goal-chunk),chunk_has_slot(A,goal,training),buffer(retrieval,_,B),chunk(B,count-order),chunk_has_slot(B,first,2),chunk_has_slot(B,second,3)\apply_rule(train2)<=>true|buffer_change(goal,chunk(_,goal-chunk,[ (start,2), (end,4), (goal,count)])),conflict_resolution.
delay-start@fire,buffer(goal,_,A),chunk(A,goal-chunk),chunk_has_slot(A,start,B),chunk_has_slot(A,count,nil)==>B\==nil|conflict_set(start).
start@buffer(goal,_,A),chunk(A,goal-chunk),chunk_has_slot(A,start,B),chunk_has_slot(A,count,nil)\apply_rule(start)<=>B\==nil|buffer_change(goal,chunk(_,_,[ (count,B)])),buffer_request(retrieval,chunk(_,count-order,[ (first,B)])),conflict_resolution.
delay-increment@fire,buffer(goal,_,A),chunk(A,goal-chunk),chunk_has_slot(A,count,C),chunk_has_slot(A,end,D),buffer(retrieval,_,B),chunk(B,count-order),chunk_has_slot(B,first,C),chunk_has_slot(B,second,E)==>C\==nil,D\==C,E\==nil|conflict_set(increment).
increment@buffer(goal,_,A),chunk(A,goal-chunk),chunk_has_slot(A,count,C),chunk_has_slot(A,end,D),buffer(retrieval,_,B),chunk(B,count-order),chunk_has_slot(B,first,C),chunk_has_slot(B,second,E)\apply_rule(increment)<=>C\==nil,D\==C,E\==nil|buffer_change(goal,chunk(_,_,[ (count,E)])),buffer_request(retrieval,chunk(_,count-order,[ (first,E)])),output(C),conflict_resolution.
delay-incrementx@fire,buffer(goal,_,A),chunk(A,goal-chunk),chunk_has_slot(A,count,C),chunk_has_slot(A,end,D),buffer(retrieval,_,B),chunk(B,count-order),chunk_has_slot(B,first,C),chunk_has_slot(B,second,E)==>C\==nil,D\==C,E\==nil|conflict_set(incrementx).
incrementx@buffer(goal,_,A),chunk(A,goal-chunk),chunk_has_slot(A,count,C),chunk_has_slot(A,end,D),buffer(retrieval,_,B),chunk(B,count-order),chunk_has_slot(B,first,C),chunk_has_slot(B,second,E)\apply_rule(incrementx)<=>C\==nil,D\==C,E\==nil|buffer_clear(goal),output(wrong),conflict_resolution.
delay-stop@fire,buffer(goal,_,A),chunk(A,goal-chunk),chunk_has_slot(A,count,B),chunk_has_slot(A,end,B)==>B\==nil|conflict_set(stop).
stop@buffer(goal,_,A),chunk(A,goal-chunk),chunk_has_slot(A,count,B),chunk_has_slot(A,end,B)\apply_rule(stop)<=>B\==nil|buffer_clear(goal),output(B),conflict_resolution.
init@run<=>true|set_default_utilities([stop,incrementx,increment,start,train2,train1]),add_buffer(retrieval,declarative_module),add_buffer(goal,declarative_module),lisp_chunktype([chunk]),lisp_sgp([:,esc,t]),lisp_chunktype([count-order,first,second]),lisp_chunktype([goal-chunk,goal,start,end,count]),lisp_adddm([[b,isa,count-order,first,1,second,2],[c,isa,count-order,first,2,second,3],[c1,isa,count-order,first,2,second,4],[d,isa,count-order,first,3,second,4],[e,isa,count-order,first,4,second,5],[f,isa,count-order,first,5,second,6],[first-goal,isa,goal-chunk,goal,training]]),lisp_goalfocus([first-goal]),lisp_spp([increment,:,u,8,incrementx,:,u,0]),lisp_spp([increment,:,reward,15,incrementx,:,reward,0]),now(0),conflict_resolution,nextcyc.
no-rule@fire<=>true|conflict_set([]),choose.


/*
?- run.
Correct to: "chr_actr:run"? yes
0: -10 ... calling event: do_conflict_resolution
going to apply rule train1
0.05:0 ... calling event: apply_rule(train1)
firing rule train1
0.05:0 ... calling event: start_request(retrieval,chunk(_G647500,count-order,[ (first,2), (second,3)]))
Started buffer request retrieval
clear buffer retrieval
0.05: -10 ... calling event: do_conflict_resolution
going to apply rule train1
0.09746853249443344:0 ... calling event: do_buffer_request(retrieval,chunk(_G18357,count-order,[ (first,2), (second,3)]))
performing request: retrieval
Retrieved chunk c
Put chunk c into buffer
0.1:0 ... calling event: apply_rule(train1)
firing rule train1
0.1:0 ... calling event: start_request(retrieval,chunk(_G28713,count-order,[ (first,2), (second,3)]))
Started buffer request retrieval
clear buffer retrieval
ERROR: put_attr/3: Uninstantiated argument expected, found 0 (1-st argument)

*/