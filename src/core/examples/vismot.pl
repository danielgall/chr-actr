 
file_search_path(chractr,'.').
:- include(chractr('actr_core.pl')).
:- chr_constraint run/0, fire/0.


delay-start@
fire,
buffer(goal,B,A),
chunk(A,goal),
chunk_has_slot(A,state,start)
==>true|
conflict_set(rule(start,[fire,buffer(goal,B,A),chunk(A,goal),chunk_has_slot(A,state,start)],[])).



start@buffer(goal,B,A),chunk(A,goal),chunk_has_slot(A,state,start)\apply_rule(rule(start,[fire,buffer(goal,B,A),chunk(A,goal),chunk_has_slot(A,state,start)],[]))<=>true|buffer_change(goal,chunk(_,_,[ (state,attend-object)])),buffer_request(manual,chunk(_,hand-to-mouse,[])),buffer_request(visual-location,chunk(_,visual-location,[])),conflict_resolution.






delay-attend-object@fire,buffer_state(visual,state,free),buffer(visual-location,E,A),chunk(A,visual-location),chunk_has_slot(A,screen-x,C),chunk_has_slot(A,screen-y,D),buffer(goal,F,B),chunk(B,goal),chunk_has_slot(B,state,attend-object)==>C\==nil,D\==nil|conflict_set(rule(attend-object,[fire,buffer_state(visual,state,free),buffer(visual-location,E,A),chunk(A,visual-location),chunk_has_slot(A,screen-x,C),chunk_has_slot(A,screen-y,D),buffer(goal,F,B),chunk(B,goal),chunk_has_slot(B,state,attend-object)],[])).
attend-object@buffer_state(visual,state,free),buffer(visual-location,E,A),chunk(A,visual-location),chunk_has_slot(A,screen-x,C),chunk_has_slot(A,screen-y,D),buffer(goal,F,B),chunk(B,goal),chunk_has_slot(B,state,attend-object)\apply_rule(rule(attend-object,[fire,buffer_state(visual,state,free),buffer(visual-location,E,A),chunk(A,visual-location),chunk_has_slot(A,screen-x,C),chunk_has_slot(A,screen-y,D),buffer(goal,F,B),chunk(B,goal),chunk_has_slot(B,state,attend-object)],[]))<=>C\==nil,D\==nil|buffer_change(goal,chunk(_,_,[ (state,read-and-move), (x,C), (y,D)])),buffer_request(visual,chunk(_,move-attention,[ (screen-pos-x,C), (screen-pos-y,D)])),conflict_resolution.
delay-read-and-move@fire,buffer(visual,F,A),chunk(A,text),chunk_has_slot(A,value,C),buffer(goal,G,B),chunk(B,goal),chunk_has_slot(B,x,D),chunk_has_slot(B,y,E),chunk_has_slot(B,state,read-and-move)==>C\==nil,D\==nil,E\==nil|conflict_set(rule(read-and-move,[fire,buffer(visual,F,A),chunk(A,text),chunk_has_slot(A,value,C),buffer(goal,G,B),chunk(B,goal),chunk_has_slot(B,x,D),chunk_has_slot(B,y,E),chunk_has_slot(B,state,read-and-move)],[])).
read-and-move@buffer(visual,F,A),chunk(A,text),chunk_has_slot(A,value,C),buffer(goal,G,B),chunk(B,goal),chunk_has_slot(B,x,D),chunk_has_slot(B,y,E),chunk_has_slot(B,state,read-and-move)\apply_rule(rule(read-and-move,[fire,buffer(visual,F,A),chunk(A,text),chunk_has_slot(A,value,C),buffer(goal,G,B),chunk(B,goal),chunk_has_slot(B,x,D),chunk_has_slot(B,y,E),chunk_has_slot(B,state,read-and-move)],[]))<=>C\==nil,D\==nil,E\==nil|buffer_request(manual,chunk(_,move-cursor,[ (loc-x,D), (loc-y,E)])),buffer_change(goal,chunk(_,_,[ (state,click), (caption,C)])),conflict_resolution.
delay-click@fire,buffer(goal,B,A),chunk(A,goal),chunk_has_slot(A,state,click)==>true|conflict_set(rule(click,[fire,buffer(goal,B,A),chunk(A,goal),chunk_has_slot(A,state,click)],[])).
click@buffer(goal,B,A),chunk(A,goal),chunk_has_slot(A,state,click)\apply_rule(rule(click,[fire,buffer(goal,B,A),chunk(A,goal),chunk_has_slot(A,state,click)],[]))<=>true|buffer_request(manual,chunk(_,click-mouse,[])),conflict_resolution.
init@
run<=>true|
add_buffer(retrieval,declarative_module),
add_buffer(goal,declarative_module),
% add visual, visual-location and manual buffers --> idea to make this more elegant??
%

lisp_chunktype([chunk]),lisp_chunktype([goal,state,caption,x,y]),lisp_adddm([[g,isa,goal,state,start]]),lisp_goalfocus([g]),set_default_utilities([click,read-and-move,attend-object,start]),now(0),conflict_resolution,nextcyc.
no-rule@fire<=>true|conflict_set([]),choose.
