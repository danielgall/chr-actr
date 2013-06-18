:- module(chr_actr,[fire/0,do_buffer_request/2,do_buffer_change/2,do_buffer_clear/1,set_production_utility/2]).
:- use_module(library(chr)).

:- use_module('scheduler.pl').
:- use_module('declarative_module.pl').
:- use_module('configuration.pl').
:- include('buffers.pl').
:- include('std_lisp.pl').

:- chr_constraint goal_focus/1, init/0, output/1, do_output/1.

goal_focus(Chunk) <=>
  declarative_module:module_request(goal,chunk(Chunk,_,_),[],ResChunk,ResState,_),
  add_chunk(ResChunk),
  set_buffer(goal,ResChunk),
  set_buffer_state(goal,ResState).
    
%now(Now) \ output(X) <=> Time is Now + 1, add_q(Time,do_output(X)).
%do_output(X) <=> write(X),nl.
output(X) <=> write(output:X),nl.  % reference p. 166: eval output, bind calls occur during fire event => immediately

% TODO
:- chr_constraint get_context/1, context/1, collect_context/1.

chunk_has_slot(_,_,V), get_context(_) ==> context([V]).
get_context(Context) <=> write('context'),collect_context(Context). % no more slots => collect results

context([nil]) <=> true.
context(C1),context(C2) <=> append(C1,C2,C), context(C).
context(C), collect_context(Context) <=> Context = C.  

:- chr_constraint set_production_utility/2, production_utility/2, conflict_set/1, apply_rule/1.

set_production_utility(P,U), production_utility(P,_) <=> 
  production_utility(P,U).
  
set_production_utility(P,U) <=> 
  production_utility(P,U).
  
collect @ conflict_set(L1), conflict_set(L2) <=>
  append(L1,L2,L), 
  conflict_set(L).

conflict_set([]) <=> write('No rule'),nl,
  true.
choose @ conflict_set([P|Ps]) <=>
  write(conflict-set:[P|Ps]),nl,
  apply_rule(P).
  
apply_rule(P) ==> write('firing rule '),write(P),nl.