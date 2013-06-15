:- module(chr_actr,[]).
:- use_module(library(chr)).

:- use_module('declarative_module.pl').
:- use_module('test_module.pl').
:- include('buffers.pl').
:- include('std_lisp.pl').

:- chr_constraint goal_focus/1, init/0, output/1, do_output/1.

goal_focus(Chunk) <=>
  declarative_module:module_request(goal,chunk(Chunk,_,_),[],ResChunk,ResState,_),
  add_chunk(ResChunk),
  set_buffer(goal,ResChunk),
  set_buffer_state(goal,ResState).
  
init <=>
    declarative_module(declarative_module).
    
%now(Now) \ output(X) <=> Time is Now + 1, add_q(Time,do_output(X)).
%do_output(X) <=> write(X),nl.
output(X) <=> write(output:X),nl.  % reference p. 166: eval output, bind calls occur during fire event => immediately


% % % % % % % % % % %
% Phases/Scheduling %
% % % % % % % % % % %

:- use_module('priority_queue.pl').

:- chr_constraint call_event(+).
% Takes a q(Time,Priority,Event) and calls Event.
% It prints the time and priority of the event and the explicit call.

:- chr_constraint nextcyc/0.
% After an event has been performed, nextcyc is triggered.
% It causes the next event in queue to be performed.

:- chr_constraint conflict_resolution/0, do_conflict_resolution/0.

:- chr_constraint no_rule/0.

% Output rule
% Informs user, that a conflict-resolution phase started
now(Now), fire ==> write(Now),write(' ... conflict resolution'),nl.

% After an event has been performed, nextcyc is triggered. This leads to the next event in the queue to be performed.
nextcyc <=> de_q(Evt), call_event(Evt).

% no event in queue -> do nothing and remove current time
call_event(nil), now(Time) <=> true.

call_event(q(Time,Priority,Evt)), now(Now) <=> Now =< Time | write(Now:Time:Priority),write(' ... '),write('calling event: '), write(Evt),nl,call(Evt),now(Time),nextcyc.

% if a production rule has been fired and has finished its actions (ie. added them to event queue), next conflict_resolution is scheduled,
% because procedural module is free now. The time of the conflict_resolution is now. Priority is low, because first the actions of the performed production rule, that take place now, should be performed, before next production is chosen.
now(Time) \ conflict_resolution <=> add_q(Time,0,do_conflict_resolution),write('q: ').

% causes a matching production rule to fire
do_conflict_resolution <=> fire.%TODO add proper conflict resolution mechanism

% no rule matched
no_rule <=> write('No rule matches -> Schedule next conflict resolution event'),nl,after_next_event(do_conflict_resolution).

% TODO
:- chr_constraint get_context/1, context/1, collect_context/1.

chunk_has_slot(_,_,V), get_context(_) ==> context([V]).
get_context(Context) <=> write('context'),collect_context(Context). % no more slots => collect results

context(C1),context(C2) <=> append(C1,C2,C), context(C).
context(C), collect_context(Context) <=> Context = C.  
