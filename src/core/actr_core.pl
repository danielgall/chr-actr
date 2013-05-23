:- module(chr_actr,[]).
:- use_module(library(chr)).

:- use_module('declarative_module.pl').
:- use_module('test_module.pl').
:- include('buffers.pl').

:- chr_constraint add_dm/1, goal_focus/1, init/0.

add_dm(C) <=> declarative_module:add_chunk(C).

goal_focus(Chunk) <=>
  declarative_module:module_request(retrieval,chunk(Chunk,_,_),ResChunk),
  ResChunk = chunk(CName,_,_),
  add_chunk(ResChunk),
  buffer(goal,declarative_module,CName).
  
init <=>
    declarative_module(declarative_module).
  
