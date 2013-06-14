:- module(chr_actr,[]).
:- use_module(library(chr)).

:- include('scheduler.pl').
:- use_module('declarative_module.pl').
:- use_module('test_module.pl').
:- include('buffers.pl').
:- include('std_lisp.pl').

:- chr_constraint goal_focus/1, init/0, output/1, do_output/1, nextcyc/0, conflict_resolution/0, do_conflict_resolution/0.

goal_focus(Chunk) <=>
  declarative_module:module_request(goal,chunk(Chunk,_,_),ResChunk,ResState,_),
  add_chunk(ResChunk),
  set_buffer(goal,ResChunk),
  set_buffer_state(goal,ResState).
  
init <=>
    declarative_module(declarative_module).
    
%now(Now) \ output(X) <=> Time is Now + 1, add_q(Time,do_output(X)).
%do_output(X) <=> write(X),nl.
output(X) <=> write(output:X),nl.  % reference p. 166: eval output, bind calls occur during fire event => immediately

:- chr_constraint call_event/1.

now(Now), fire ==> write(Now),write(' ... conflict resolution'),nl.

nextcyc <=> de_q(Evt), call_event(Evt).

call_event(nil), now(Time) <=> true.

call_event(q(Time,Priority,Evt)), now(Now) <=> Now =< Time | write(Now:Time:Priority),write(' ... '),write('calling event: '), write(Evt),nl,call(Evt),now(Time),nextcyc.

now(Time) \ conflict_resolution <=> add_q(Time,0,do_conflict_resolution),write('q: '),print_q(s),nl.

do_conflict_resolution <=> fire.%TODO add proper conflict resolution mechanism

  
