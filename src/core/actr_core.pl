:- module(chr_actr,[]).
:- use_module(library(chr)).

:- include('scheduler.pl').
:- use_module('declarative_module.pl').
:- use_module('test_module.pl').
:- include('buffers.pl').

:- chr_constraint goal_focus/1, init/0, output/1, do_output/1.

goal_focus(Chunk) <=>
  declarative_module:module_request(goal,chunk(Chunk,_,_),ResChunk),
  add_chunk(ResChunk),
  set_buffer(goal,ResChunk).
  
init <=>
    declarative_module(declarative_module).
    
now(Now) \ output(X) <=> Time is Now + 1, add_q(Time,do_output(X)).

do_output(X) <=> write(X),nl.

:- chr_constraint call_event/1.

no-rule @ 
nextcyc <=> de_q(Evt), call_event(Evt).

call_event(nil), now(Time) <=> now(Time),fire.

call_event(q(Time,Evt)), now(_) <=> write(Time),write(' ... '),write('calling event: '), write(Evt),nl,call(Evt),now(Time),nextcyc.

nextcyc <=> fire.
  
