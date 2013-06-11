:- use_module(library(chr)).


% very simple priority queue
% sorts time-events pairs by time
% if two events have the same time, they will be set in order in unpredictable order

:- op(900, xfx, ['-->']).
  
:- chr_constraint (-->)/2, add_q/2, de_q/1.

add_q(Time,Evt) <=>
  s --> q(Time,Evt).

 
A --> A <=> A \== s | true.

_ --> s <=> false.

A --> B, A --> C <=>
  leq(B,C) |
  A --> B,
  B --> C.  
  
  
leq(s,_).
leq(q(Time1,_), q(Time2,_)) :-
  Time1 =< Time2.
  
de_q(X), s --> A, A --> B <=>
  X = A,
  s --> B.

de_q(X), s --> A <=>
  X = A.  