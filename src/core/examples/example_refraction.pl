file_search_path(chractr,'..').
:- include(chractr('actr_core.pl')).
:- chr_constraint run/0, fire/0.

% This example can be started with two initial goals, c1 and c2.
% If started with c1, the program will not terminate and calls 
% the rule first again and again with the default conflict resolution
% mechanism. When using the conflict resolution with refraction, the
% program terminates after one execution of rule first.
% If started with c2, the program does only terminate, if the conflict resolution
% with refraction does not use the history cleanup for new instances of with the same values
% (i.e. rules can fire again for the same instantiation, if the parts of the instantiation have
% been modified and reset again). In all other cases (default conflict resolution, conflict resolution 
% with refraction and history cleanup) the program does not terminate and calls rules 
% second1 and second2 alternately.

delay-first@fire,buffer(goal,B,A),chunk(A,t),chunk_has_slot(A,s1,v)==>true|conflict_set(rule(first,[fire,buffer(goal,B,A),chunk(A,t),chunk_has_slot(A,s1,v)],[])).
first@buffer(goal,B,A),chunk(A,t),chunk_has_slot(A,s1,v)\apply_rule(rule(first,[fire,buffer(goal,B,A),chunk(A,t),chunk_has_slot(A,s1,v)],[]))<=>true|output(out),conflict_resolution.
delay-second1@fire,buffer(goal,B,A),chunk(A,t),chunk_has_slot(A,s1,v1)==>true|conflict_set(rule(second1,[fire,buffer(goal,B,A),chunk(A,t),chunk_has_slot(A,s1,v1)],[])).
second1@buffer(goal,B,A),chunk(A,t),chunk_has_slot(A,s1,v1)\apply_rule(rule(second1,[fire,buffer(goal,B,A),chunk(A,t),chunk_has_slot(A,s1,v1)],[]))<=>true|output(out),buffer_change(goal,chunk(_,_,[ (s1,v2)])),conflict_resolution.
delay-second2@fire,buffer(goal,B,A),chunk(A,t),chunk_has_slot(A,s1,v2)==>true|conflict_set(rule(second2,[fire,buffer(goal,B,A),chunk(A,t),chunk_has_slot(A,s1,v2)],[])).
second2@buffer(goal,B,A),chunk(A,t),chunk_has_slot(A,s1,v2)\apply_rule(rule(second2,[fire,buffer(goal,B,A),chunk(A,t),chunk_has_slot(A,s1,v2)],[]))<=>true|output(out),buffer_change(goal,chunk(_,_,[ (s1,v1)])),conflict_resolution.
init@run<=>true|add_buffer(retrieval,declarative_module),add_buffer(goal,declarative_module),lisp_chunktype([chunk]),lisp_chunktype([t,s1]),lisp_adddm([[c1,isa,t,s1,v],[c2,isa,t,s1,v1]]),lisp_goalfocus([c2]),set_default_utilities([second2,second1,first]),now(0),conflict_resolution,nextcyc.
no-rule@fire<=>true|conflict_set([]),choose.
