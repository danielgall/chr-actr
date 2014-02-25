file_search_path(chractr,'.').
:- include(chractr('actr_core.pl')).
:- chr_constraint run/0, run/1, fire/0, sample/1.

/*
 * This model is designed to play against an opponent with pre-defined moves in the rock paper scissors experiment.
 * It has been derived from the corresponding ACT-R model.
 * It is configured to add the subsymbolic information of the ACT-R 5.0 conflict resolution mechanism (i.e. successes, costs etc.) and
 * will fail when trying to load it with the ACT-R 6.0 configuration (i.e. conflict_resolution.pl). It also works for conflict_resolution_optimist.pl.
 * 
 * Usage:
 *   The sample of the opponent is given as a sample(Xs) constraint where Xs is an arbitrarily long list consisting only of the elements
 *   rock, paper and scissors. The opponent processes this list interpreting the elements as his move.
 * Example goal:
 *   sample([paper, rock, scissors, rock]), run.
 *   This goal simulates a game against an opponent playing first paper, then rock etc.
 */


delay-play-rock@fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,nil),chunk_has_slot(A,opponent,nil)==>true|conflict_set(rule(play-rock,[fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,nil),chunk_has_slot(A,opponent,nil)],[])).

play-rock@sample(L),
buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,nil),chunk_has_slot(A,opponent,nil),
chunk_has_slot(A,round,Round)
\apply_rule(rule(play-rock,[fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,nil),chunk_has_slot(A,opponent,nil)],[]))<=>
nth0(Round,L,X), Round1 is Round+1 | output(rock),output(X),buffer_change(goal,chunk(_,_,[ (me,rock), (opponent,X),
(round,Round1)])),conflict_resolution.


delay-play-paper@fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,nil),chunk_has_slot(A,opponent,nil)==>true|conflict_set(rule(play-paper,[fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,nil),chunk_has_slot(A,opponent,nil)],[])).

play-paper@sample(L),
buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,nil),chunk_has_slot(A,opponent,nil),
chunk_has_slot(A,round,Round)
\apply_rule(rule(play-paper,[fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,nil),chunk_has_slot(A,opponent,nil)],[]))<=>
nth0(Round,L,X), Round1 is Round+1 |output(paper),output(X),buffer_change(goal,chunk(_,_,[ (me,paper), (opponent,X),
(round,Round1)])),conflict_resolution.


delay-play-scissors@fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,nil),chunk_has_slot(A,opponent,nil)==>true|conflict_set(rule(play-scissors,[fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,nil),chunk_has_slot(A,opponent,nil)],[])).

play-scissors@ sample(L),
buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,nil),chunk_has_slot(A,opponent,nil),
chunk_has_slot(A,round,Round)
\apply_rule(rule(play-scissors,[fire,buffer(goal,B,A),chunk(A,game),chunk_has_slot(A,me,nil),chunk_has_slot(A,opponent,nil)],[]))<=>
nth0(Round,L,X),writeln(Round), Round1 is Round+1, writeln(Round1) | 
output(scissors),output(X),buffer_change(goal,chunk(_,_,[ (me,scissors), (opponent,X), 
(round,Round1)])),
conflict_resolution.


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

init@run<=>true|add_buffer(retrieval,declarative_module),add_buffer(goal,declarative_module),lisp_chunktype([chunk]),lisp_sgp([:,esc,t,:,v,t,:,ul,t,:,ult,t]),lisp_chunktype([game,me,opponent,round]),lisp_adddm([[goal,isa,game,me,nil,opponent,nil,round,0]]),lisp_spp([recognize-win1,:,success,t]),set_default_utilities([recognize-defeat3,recognize-defeat2,recognize-defeat1,recognize-draw,recognize-win3,recognize-win2,recognize-win1,play-scissors,play-paper,play-rock]),lisp_spp([recognize-win2,:,success,t]),lisp_spp([recognize-win3,:,success,t]),lisp_spp([recognize-defeat1,:,failure,t]),lisp_spp([recognize-defeat2,:,failure,t]),lisp_spp([recognize-defeat3,:,failure,t]),lisp_goalfocus([goal]),now(0),conflict_resolution,nextcyc.
no-rule@fire<=>true|conflict_set([]),choose.