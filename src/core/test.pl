:- include('actr_core.pl').

:- chr_constraint run/0, fire/0,nextcyc/0.
run <=> 
  init,
  add_buffer(retrieval,declarative_module),
  add_buffer(goal,declarative_module),
  add_chunk_type(countorder, [first, second]),
  add_chunk_type(countfrom, [start,end,count]),
  add_dm(chunk(b, countorder, [(first,1),(second,2)])),
  add_dm(chunk(c, countorder, [(first,2),(second,3)])),
  add_dm(chunk(d, countorder, [(first,3),(second,4)])),
  add_dm(chunk(e, countorder, [(first,4),(second,5)])),
  add_dm(chunk(f, countorder, [(first,5),(second,6)])),
  add_dm(chunk(first-goal, countfrom, [(start,2),(end,4),(count,nil)])),
  goal_focus(first-goal), now(0), fire.

start @ 
  buffer(goal,_,A),
    chunk(A,countfrom),
    chunk_has_slot(A,start,B),
    chunk_has_slot(A,count,nil) \ fire
<=> true|
  buffer_change(goal,chunk(C,D,[ (count,B)])),
  buffer_request(retrieval,chunk(E,countorder,[ (first,B)])),nextcyc.
  
increment@
  buffer(goal,_,A),
    chunk(A,countfrom),
    chunk_has_slot(A,count,B),
    chunk_has_slot(A,end,C),
  buffer(retrieval,_,D),
    chunk(D,countorder),
    chunk_has_slot(D,first,B),
    chunk_has_slot(D,second,E) \ fire
<=>C\==B|
  buffer_change(goal,chunk(F,G,[ (count,E)])),
  buffer_request(retrieval,chunk(H,countorder,[ (first,E)])),
  output(B),nextcyc.

stop@
buffer(goal,_,A),
chunk(A,countfrom),chunk_has_slot(A,count,B),chunk_has_slot(A,end,B) \ fire<=>
true|buffer_clear(goal),output(B),nextcyc.
