%:- module(chr_actr,[fire/0,start_request/2,do_buffer_request/2,do_buffer_change/2,do_buffer_clear/1,set_production_utility/2,apply_rule/1]).
:- use_module(library(chr)).

:- use_module('scheduler.pl').
:- use_module('declarative_module.pl').
:- use_module('configuration.pl').
:- include('buffers.pl').
:- include('std_lisp.pl').

:- chr_constraint goal_focus/1, output/1, do_output/1.

goal_focus(Chunk) <=>
  declarative_module:module_request(goal,chunk(Chunk,_,_),[],ResChunk,ResState,_),
  add_chunk(ResChunk),
  set_buffer(goal,ResChunk),
  set_buffer_state(goal,ResState).
    
output(X) <=> write(output:X),nl.  % reference p. 166: eval output, bind calls occur during fire event => immediately

%
% Rules to calculate the current context, i.e. the chunks in the buffers
%
:- chr_constraint get_context/1, context/1, collect_context/1.

chunk_has_slot(_,_,V), get_context(_) ==> context([V]).
get_context(Context) <=> collect_context(Context). % no more slots => collect results

context([nil]) <=> true.
context(C1),context(C2) <=> append(C1,C2,C), context(C).
context(C), collect_context(Context) <=> Context = C.  

% uncomment desired conflict resolution mechanism
:- include('conflict_resolution.pl').
%:- include('conflict_resolution_old.pl').
%:- include('conflict_resolution_optimist.pl').
%:- include('conflict_resolution_refraction.pl').