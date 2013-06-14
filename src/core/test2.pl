:- include('actr_core.pl').
:- chr_constraint run/0, fire/0.


initialize-addition@buffer(goal,A),chunk(A,add),chunk_has_slot(A,arg1,B),chunk_has_slot(A,arg2,_),chunk_has_slot(A,sum,nil)\fire<=>true|buffer_change(goal,chunk(_,_,[ (sum,B), (count,0)])),buffer_request(retrieval,chunk(_,count-order,[ (first,B)])),fire.
terminate-addition@buffer(goal,A),chunk(A,add),chunk_has_slot(A,count,B),chunk_has_slot(A,arg2,B),chunk_has_slot(A,sum,_)\fire<=>true|buffer_change(goal,chunk(_,_,[ (count,nil)])),fire.
increment-count@buffer(goal,A),chunk(A,add),chunk_has_slot(A,sum,E),chunk_has_slot(A,count,C),buffer(retrieval,B),chunk(B,count-order),chunk_has_slot(B,first,C),chunk_has_slot(B,second,D)\fire<=>true|buffer_change(goal,chunk(_,_,[ (count,D)])),buffer_request(retrieval,chunk(_,count-order,[ (first,E)])),fire.
increment-sum@buffer(goal,A),chunk(A,add),chunk_has_slot(A,sum,C),chunk_has_slot(A,count,E),chunk_has_slot(A,arg2,D),buffer(retrieval,B),chunk(B,count-order),chunk_has_slot(B,first,C),chunk_has_slot(B,second,F)\fire<=>D\==E|buffer_change(goal,chunk(_,_,[ (sum,F)])),buffer_request(retrieval,chunk(_,count-order,[ (first,E)])),fire.
init@run<=>true|
lisp_chunk-type([count-order,first,second]),lisp_chunk-type([add,arg1,arg2,sum,count]),
lisp_adddm([[a,isa,count-order,first,0,second,1],[b,isa,count-order,first,1,second,2],
[c,isa,count-order,first,2,second,3],[d,isa,count-order,first,3,second,4],[e,isa,count-order,first,4,second,5],[f,isa,count-order,first,5,second,6],
[g,isa,count-order,first,6,second,7],[h,isa,count-order,first,7,second,8],[i,isa,count-order,first,8,second,9],[j,isa,count-order,first,9,second,10],
[second-goal,isa,add,arg1,5,arg2,2]]),lisp_goal-focus([second-goal]).
