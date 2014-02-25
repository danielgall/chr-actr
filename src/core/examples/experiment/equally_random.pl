file_search_path(chractr,'.').
:- include(chractr('actr_core.pl')).
:- chr_constraint run/0, run/1, fire/0.

/*
 * This model is designed to play against an opponent choosing equally from rock, paper and scissors in the rock paper scissors experiment.
 * It has been derived from the corresponding ACT-R model.
 * It is configured to add the subsymbolic information of the ACT-R 6.0 conflict resolution mechanism (i.e. rewards etc.) and
 * will fail when trying to load it with the ACT-R 5.0 configuration (i.e. conflict_resolution_old.pl).
 * Note that this randomized experiment has been replaced by an experiment with pre-defined (randomly generated) samples in the paper.
 * The files expsample.pl and expsample5.pl are able to receive a sample and performing the experiment as described in the paper.
 */

delay-play-scissors@fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,nil),chunk_has_slot(A,opponent,nil)==>true|conflict_set(rule(play-scissors,[fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,nil),chunk_has_slot(A,opponent,nil)],[])).
play-scissors@buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,nil),chunk_has_slot(A,opponent,nil)\apply_rule(rule(play-scissors,[fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,nil),chunk_has_slot(A,opponent,nil)],[]))<=>L=[rock,paper,scissors],length(L,N),random_between(1,N,I),nth1(I,L,X) | output(scissors),output(X),buffer_change(goal,chunk(_,_,[ (me,scissors), (opponent,X)])),conflict_resolution.

delay-play-paper@fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,nil),chunk_has_slot(A,opponent,nil)==>true|conflict_set(rule(play-paper,[fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,nil),chunk_has_slot(A,opponent,nil)],[])).
play-paper@buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,nil),chunk_has_slot(A,opponent,nil)\apply_rule(rule(play-paper,[fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,nil),chunk_has_slot(A,opponent,nil)],[]))<=>L=[rock,paper,scissors],length(L,N),random_between(1,N,I),nth1(I,L,X) |output(paper),output(X),buffer_change(goal,chunk(_,_,[ (me,paper), (opponent,X)])),conflict_resolution.

delay-play-rock@fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,nil),chunk_has_slot(A,opponent,nil)==>true|conflict_set(rule(play-rock,[fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,nil),chunk_has_slot(A,opponent,nil)],[])).
play-rock@buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,nil),chunk_has_slot(A,opponent,nil)\apply_rule(rule(play-rock,[fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,nil),chunk_has_slot(A,opponent,nil)],[]))<=>L=[rock,paper,scissors],length(L,N),random_between(1,N,I),nth1(I,L,X) | output(rock),output(X),buffer_change(goal,chunk(_,_,[ (me,rock), (opponent,X)])),conflict_resolution.

delay-recognize-win1@fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,rock),chunk_has_slot(A,opponent,scissors)==>true|conflict_set(rule(recognize-win1,[fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,rock),chunk_has_slot(A,opponent,scissors)],[])).
recognize-win1@buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,rock),chunk_has_slot(A,opponent,scissors)\apply_rule(rule(recognize-win1,[fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,rock),chunk_has_slot(A,opponent,scissors)],[]))<=>true|output(me),buffer_change(goal,chunk(_,_,[ (me,nil), (opponent,nil)])),conflict_resolution.
delay-recognize-win2@fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,paper),chunk_has_slot(A,opponent,rock)==>true|conflict_set(rule(recognize-win2,[fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,paper),chunk_has_slot(A,opponent,rock)],[])).
recognize-win2@buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,paper),chunk_has_slot(A,opponent,rock)\apply_rule(rule(recognize-win2,[fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,paper),chunk_has_slot(A,opponent,rock)],[]))<=>true|output(me),buffer_change(goal,chunk(_,_,[ (me,nil), (opponent,nil)])),conflict_resolution.
delay-recognize-win3@fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,scissors),chunk_has_slot(A,opponent,paper)==>true|conflict_set(rule(recognize-win3,[fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,scissors),chunk_has_slot(A,opponent,paper)],[])).
recognize-win3@buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,scissors),chunk_has_slot(A,opponent,paper)\apply_rule(rule(recognize-win3,[fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,scissors),chunk_has_slot(A,opponent,paper)],[]))<=>true|output(me),buffer_change(goal,chunk(_,_,[ (me,nil), (opponent,nil)])),conflict_resolution.
delay-recognize-draw@fire,buffer(goal,C,A),chunk(A,game),chunk_has_slot(A,opponent,B),chunk_has_slot(A,me,B)==>B\==nil|conflict_set(rule(recognize-draw,[fire,buffer(goal,C,A),chunk(A,game),chunk_has_slot(A,opponent,B),chunk_has_slot(A,me,B)],[])).
recognize-draw@buffer(goal,C,A),chunk(A,game),chunk_has_slot(A,opponent,B),chunk_has_slot(A,me,B)\apply_rule(rule(recognize-draw,[fire,buffer(goal,C,A),chunk(A,game),chunk_has_slot(A,opponent,B),chunk_has_slot(A,me,B)],[]))<=>B\==nil|output(draw),buffer_change(goal,chunk(_,_,[ (me,nil), (opponent,nil)])),conflict_resolution.
delay-recognize-defeat1@fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,scissors),chunk_has_slot(A,opponent,rock)==>true|conflict_set(rule(recognize-defeat1,[fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,scissors),chunk_has_slot(A,opponent,rock)],[])).
recognize-defeat1@buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,scissors),chunk_has_slot(A,opponent,rock)\apply_rule(rule(recognize-defeat1,[fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,scissors),chunk_has_slot(A,opponent,rock)],[]))<=>true|output(opponent),buffer_change(goal,chunk(_,_,[ (me,nil), (opponent,nil)])),conflict_resolution.
delay-recognize-defeat2@fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,rock),chunk_has_slot(A,opponent,paper)==>true|conflict_set(rule(recognize-defeat2,[fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,rock),chunk_has_slot(A,opponent,paper)],[])).
recognize-defeat2@buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,rock),chunk_has_slot(A,opponent,paper)\apply_rule(rule(recognize-defeat2,[fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,rock),chunk_has_slot(A,opponent,paper)],[]))<=>true|output(opponent),buffer_change(goal,chunk(_,_,[ (me,nil), (opponent,nil)])),conflict_resolution.
delay-recognize-defeat3@fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,paper),chunk_has_slot(A,opponent,scissors)==>true|conflict_set(rule(recognize-defeat3,[fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,paper),chunk_has_slot(A,opponent,scissors)],[])).
recognize-defeat3@buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,paper),chunk_has_slot(A,opponent,scissors)\apply_rule(rule(recognize-defeat3,[fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,paper),chunk_has_slot(A,opponent,scissors)],[]))<=>true|output(opponent),buffer_change(goal,chunk(_,_,[ (me,nil), (opponent,nil)])),conflict_resolution.

run(X) <=> stopat(X), run.

init@run<=>true|add_buffer(retrieval,declarative_module),add_buffer(goal,declarative_module),lisp_chunktype([chunk]),lisp_sgp([:,esc,t,:,v,t,:,ul,t,:,ult,t]),lisp_chunktype([game,me,opponent]),lisp_adddm([[goal,isa,game,me,nil,opponent,nil]]),lisp_spp([recognize-win1,:,reward,2]),set_default_utilities([recognize-defeat3,recognize-defeat2,recognize-defeat1,recognize-draw,recognize-win3,recognize-win2,recognize-win1,play-scissors,play-paper,play-rock]),lisp_spp([recognize-win2,:,reward,2]),lisp_spp([recognize-win3,:,reward,2]),lisp_spp([recognize-defeat1,:,reward,0]),lisp_spp([recognize-defeat2,:,reward,0]),lisp_spp([recognize-defeat3,:,reward,0]),lisp_goalfocus([goal]),now(0),conflict_resolution,nextcyc.
no-rule@fire<=>true|conflict_set([]),choose.
 
