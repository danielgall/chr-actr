:- module(priority_queue, [add_q/3,de_q/1,after_next_event/1,remove/1,print_q/0]).
:- use_module(library(chr)).


% very simple priority queue
% sorts time-event pairs by time
% if two events have the same time, they will be set in unpredictable order

:- op(900, xfx, ['-->']).
  
:- chr_constraint (-->)/2, add_q/3, de_q/1, after_next_event/1,remove/1.

add_q(Time,Priority,Evt) <=>
  %write('added event '),
  %write(Time:Priority:Evt),nl,
  s --> q(Time,Priority,Evt).

 
A --> A <=> A \== s | true.

_ --> s <=> false.

A --> B, A --> C <=>
  leq(A,B),
  leq(B,C) |
  A --> B,
  B --> C.  
  
  
leq(s,_).
leq(q(Time1,_,_), q(Time2,_,_)) :- % Time1 < Time2 -> event with time1 first, priority does not matter
  Time1 < Time2.
leq(q(Time,Priority1,_), q(Time,Priority2,_)) :- % same time: event with higher priority first
  Priority1 >= Priority2.
  
de_q(X), s --> A, A --> B <=>
  X = A,
  s --> B.

de_q(X), s --> A <=>
  X = A.  
  
de_q(X) <=> X = nil.

s --> q(Time,P1,E1) \ after_next_event(Evt) <=> 
  NP1 is P1 + 2, % increase priority of first event, so it still has highest priority
  P is P1 + 1, % priority of event that is added, ensured that it is higher than of the former second event (because it is P1+1)
  de_q(_), % remove head of queue
  add_q(Time,NP1,E1), % add head of queue again with new priority. Will be first again, because it has old prio (which is higher than prio of all successors)
  add_q(Time,P,Evt). % add new event. Will be < than Prio of head but it is ensured that it is higher than prio of second event

% remove(Pattern)  
remove(Pattern) \ A --> B, B --> C <=> unifiable(q(_,_,Pattern),B,_) | A --> C.
  
remove(Pattern) \ _ --> B <=> unifiable(q(_,_,Pattern),B,_) | true.

remove(_) <=> true.

  
:- chr_constraint print_q/0, print_q/1, print_q/2.  

print_q <=> print_q(L), write(L),nl.
  
print_q(L) <=> print_q(s,L).
A --> B \ print_q(A,L) <=> L=[B|R],print_q(B,R).
print_q(_,R) <=> R=[].
