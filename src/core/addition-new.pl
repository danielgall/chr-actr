:- include('actr_core.pl').
:- chr_constraint run/0, fire/0.


delay-initialize-addition@fire,buffer(goal,_,A),chunk(A,add),chunk_has_slot(A,arg1,B),chunk_has_slot(A,arg2,C),chunk_has_slot(A,sum,nil)==>B\==nil,C\==nil|conflict_set(initialize-addition).
initialize-addition@buffer(goal,_,A),chunk(A,add),chunk_has_slot(A,arg1,B),chunk_has_slot(A,arg2,C),chunk_has_slot(A,sum,nil)\apply_rule(initialize-addition)<=>B\==nil,C\==nil|buffer_change(goal,chunk(_,_,[ (sum,B), (count,0)])),buffer_request(retrieval,chunk(_,count-order,[ (first,B)])),conflict_resolution.
delay-terminate-addition@fire,buffer(goal,_,A),chunk(A,add),chunk_has_slot(A,count,B),chunk_has_slot(A,arg2,B),chunk_has_slot(A,sum,C)==>B\==nil,C\==nil|conflict_set(terminate-addition).
terminate-addition@buffer(goal,_,A),chunk(A,add),chunk_has_slot(A,count,B),chunk_has_slot(A,arg2,B),chunk_has_slot(A,sum,C)\apply_rule(terminate-addition)<=>B\==nil,C\==nil|buffer_change(goal,chunk(_,_,[ (count,nil)])),conflict_resolution.
delay-increment-count@fire,buffer(goal,_,A),chunk(A,add),chunk_has_slot(A,sum,D),chunk_has_slot(A,count,C),buffer(retrieval,_,B),chunk(B,count-order),chunk_has_slot(B,first,C),chunk_has_slot(B,second,E)==>D\==nil,C\==nil,E\==nil|conflict_set(increment-count).
increment-count@buffer(goal,_,A),chunk(A,add),chunk_has_slot(A,sum,D),chunk_has_slot(A,count,C),buffer(retrieval,_,B),chunk(B,count-order),chunk_has_slot(B,first,C),chunk_has_slot(B,second,E)\apply_rule(increment-count)<=>D\==nil,C\==nil,E\==nil|buffer_change(goal,chunk(_,_,[ (count,E)])),buffer_request(retrieval,chunk(_,count-order,[ (first,D)])),conflict_resolution.
delay-increment-sum@fire,buffer(goal,_,A),chunk(A,add),chunk_has_slot(A,sum,C),chunk_has_slot(A,count,D),chunk_has_slot(A,arg2,E),buffer(retrieval,_,B),chunk(B,count-order),chunk_has_slot(B,first,C),chunk_has_slot(B,second,F)==>C\==nil,D\==nil,E\==D,F\==nil|conflict_set(increment-sum).
increment-sum@buffer(goal,_,A),chunk(A,add),chunk_has_slot(A,sum,C),chunk_has_slot(A,count,D),chunk_has_slot(A,arg2,E),buffer(retrieval,_,B),chunk(B,count-order),chunk_has_slot(B,first,C),chunk_has_slot(B,second,F)\apply_rule(increment-sum)<=>C\==nil,D\==nil,E\==D,F\==nil|buffer_change(goal,chunk(_,_,[ (sum,F)])),buffer_request(retrieval,chunk(_,count-order,[ (first,D)])),conflict_resolution.
init@run<=>true|set_default_utilities([increment-sum,increment-count,terminate-addition,initialize-addition]),add_buffer(retrieval,declarative_module),add_buffer(goal,declarative_module),
lisp_chunktype([count-order,first,second]),
lisp_chunktype([add,arg1,arg2,sum,count]),
lisp_chunktype([chunk]),
lisp_adddm([[a,isa,count-order,first,0,second,1],[b,isa,count-order,first,1,second,2],[c,isa,count-order,first,2,second,3],[d,isa,count-order,first,3,second,4],[e,isa,count-order,first,4,second,5],
[f,isa,count-order,first,5,second,6],[g,isa,count-order,first,6,second,7],[h,isa,count-order,first,7,second,8],[i,isa,count-order,first,8,second,9],[j,isa,count-order,first,9,second,10],
[0,isa,chunk],
[1,isa,chunk],
[2,isa,chunk],
[3,isa,chunk],
[4,isa,chunk],
[5,isa,chunk],
[6,isa,chunk],
[7,isa,chunk],
[8,isa,chunk],
[9,isa,chunk],
[10,isa,chunk],
[second-goal,isa,add,arg1,5,arg2,2]]),lisp_goalfocus([second-goal]),lisp_sgp([:,esc,t]),now(0),conflict_resolution,nextcyc.
no-rule@fire<=>true|conflict_set([]),choose.
