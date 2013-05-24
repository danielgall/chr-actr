:- module(chr_actr,[]).
:- use_module(library(chr)).

:- use_module('declarative_module.pl').
:- use_module('test_module.pl').
:- include('buffers.pl').

:- chr_constraint goal_focus/1, init/0, output/1.

goal_focus(Chunk) <=>
  declarative_module:module_request(retrieval,chunk(Chunk,_,_),ResChunk),
  ResChunk = chunk(CName,_,_),
  add_chunk(ResChunk),
  buffer(goal,declarative_module,CName).
  
init <=>
    declarative_module(declarative_module).
    
output(X) <=> write(X),nl.
  
