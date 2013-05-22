:- module(chr_actr,[]).
:- use_module(library(chr)).

:- use_module('declarative_module.pl').
:- include('buffers.pl').

:- chr_constraint add_dm/1, goal_focus/1.

add_dm(C) <=> declarative_module:add_chunk(C).

goal_focus(Chunk) <=>
  declarative_module:module_request(retrieval,Chunk,ResChunk),
  ResChunk = chunk(CName,_,_),
  add_chunk(ResChunk),
  buffer(goal,_,CName).
  
