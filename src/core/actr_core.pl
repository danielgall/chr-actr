:- module(chr_actr,[]).
:- use_module(library(chr)).

:- use_module('declarative_module.pl').
:- use_module('test_module.pl').
:- include('buffers.pl').

:- chr_constraint goal_focus/1, init/0, output/1.

goal_focus(Chunk) <=>
  declarative_module:module_request(goal,chunk(Chunk,_,_),ResChunk),
  add_chunk(ResChunk),
  set_buffer(goal,ResChunk).
  
init <=>
    declarative_module(declarative_module).
    
output(X) <=> write(X),nl.
  
