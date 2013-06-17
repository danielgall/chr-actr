:- module('scheduler',[add_q/3,de_q/1,after_next_event/1,get_now/1,now/1,conflict_resolution/0,nextcyc/0,no_rule/0]).

:- use_module(library(chr)).

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

:- chr_constraint no_rule/0, now/1.

% Output rule
% Informs user, that a conflict-resolution phase started
% now(Now), fire ==> write(Now),write(' ... conflict resolution'),nl.

% After an event has been performed, nextcyc is triggered. This leads to the next event in the queue to be performed.
nextcyc <=> de_q(Evt), call_event(Evt).

% no event in queue -> do nothing and remove current time
call_event(nil), now(_) <=> write('Hello'),nl,true.

call_event(q(Time,Priority,Evt)), now(Now) <=> Now =< Time | write(Now:Time:Priority),now(Time),write(yeah:Time),nl,write(' ... '),write('calling event: '), write(Evt),nl,call(Evt),nextcyc.

% if a production rule has been fired and has finished its actions (ie. added them to event queue), next conflict_resolution is scheduled,
% because procedural module is free now. The time of the conflict_resolution is now. Priority is low, because first the actions of the performed production rule, that take place now, should be performed, before next production is chosen.
now(Time) \ conflict_resolution <=> add_q(Time,0,do_conflict_resolution),write('q: ').

% causes a matching production rule to fire
do_conflict_resolution <=> write('FIRE'),nl,fire.%TODO add proper conflict resolution mechanism

% no rule matched
no_rule <=> write('No rule matches -> Schedule next conflict resolution event'),nl,after_next_event(do_conflict_resolution).

:- chr_constraint get_now/1.

now(Now) \ get_now(T) <=> T = Now,write(nowww:Now),nl.