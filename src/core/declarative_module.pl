% Implements interface "module"
:- module(declarative_module, [add_dm/1]).
:- use_module(library(chr)).

:- include('chunk_management.pl').

:- use_module('scheduler.pl').
:- use_module('configuration.pl').

%%%%%%%%%%%%%%
% Data Types %
%%%%%%%%%%%%%%

% derived from chunk_management

%%%%%%%%%%%%%%%%%%%%
% Data Constraints %
%%%%%%%%%%%%%%%%%%%%

% derived from chunk_management

% Chunk Activation

:- chr_constraint presentation(+,+).
% presentation(Chunk, Time)
% represents a presentation of chunk Chunk at time Time.
% A presentation can be triggered by using present/1.
% This is usually the case, when a chunk enters the declarative memory.
% Consider chunk merging!


% Helper:
:- chr_constraint threshold/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%
% Procedural Constraints %
%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% public


:- chr_constraint add_dm(+chunk_def).
% add_dm(+Chunk)
% Adds a chunk defined by Chunk to declarative memory.
% The argument Chunk must be of type chunk.

%
% private
:- chr_constraint check_slots(+,+).

:- chr_constraint find_chunk(?,?,?).
%find_chunk(Name,Type,Slots)

:- chr_constraint test_slots(+,+slot_list).
%test_slots(ChunkName,Slots)
%tests if chunk with name ChunkName matches all the slots in Slots for every chunk in match_set.
%If a slot does not match, the chunk is removed from match set.
%Matching means: for every pair (S,V) in Slots, the chunk must have a corresponding 

:- chr_constraint collect_matches(-).
% collects all the elements in a match_set constraint

:- chr_constraint match_set(+list(any)).

% Chunk Activation

:- chr_constraint max/2, get_max/2.

:- chr_constraint present(+chunk_def).

:- chr_constraint calc_activation/2.
:- chr_constraint base_level_part(+,+,?,-).
% base_level_part(Chunk,Time,InterimResult,BaseLevelActivation).
% A part of the base level calculation of chunk Chunk.
% all such parts of a chunk are summed up and the log of the sum is the BaseLevelActivation.
% call:
% If base level activation of Chunk C should be calculated, each presentation of a chunk will 
% add a base level part. Time is set to the time from this presentation to now.
% For every part, the InterimResult is set to that time difference Time^(-D), where D is the decay parameter.
% then all base level parts are summed up and in the end the logarithm of the sum is calculated.
% This result is bound to BaseLevelActivation.

:- chr_constraint calc_activations/2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implemented abstract constraints from interface "module" %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- chr_constraint module_request(+,+chunk_def,+,-chunk_def,-,-).
% module_request(BufName,Chunk,ContextResChunk,ResState,RelTime)

% add_chunk_type(ChunkTypeName, [SlotNames]).

:- chr_constraint init(+).

% Observer interface:
:- chr_constraint update/0.
% this method is called by the observable, where this module has been registered by using add_config_observer.

%%%%%%%%%
% Rules %
%%%%%%%%%

%%% implmented interfaces %%%

% constructor, called by buffers.pl on add_buffer. 
% Adds observer for esc setting (esc determines if subsymbolic layer is used)
init(Buffer) <=> % implements interface Module
  Buffer=retrieval |
  add_config_observer(declarative_module,esc). % observe esc setting

% configuration variable has changed  
update <=> % implements interface Observer
  get_conf(esc,ESC),
  set_subsymbolic(ESC).

%%%%

add_dm(ChunkDef) <=> add_chunk(ChunkDef), present(ChunkDef), write('!!!!!!added chunk '),write(ChunkDef),nl,nl.

:- chr_constraint fan/2, calc_sji/3, subsymbolic/0, set_subsymbolic/1.

% subsymbolic layer turned on -> add subsymbolic constraint
set_subsymbolic(t), subsymbolic <=> subsymbolic.
set_subsymbolic(t) <=> subsymbolic.

% subsymbolic layer turned off -> remove subsymbolic constraint
set_subsymbolic(nil), subsymbolic <=> true.
set_subsymbolic(nil) <=> true.

% Calculate Fan of each chunk, if subsymbolic layer is turned on
subsymbolic, chunk(C,_) ==> fan(C,1).
subsymbolic, chunk_has_slot(_,_,C), chunk(C,_) ==> fan(C,1).

subsymbolic \ fan(C,F1), fan(C,F2) <=> F is F1+F2, fan(C,F).


% Calculate S_ji, if subsymbolic layer is turned on and calc_sji is in store. Bind calculated value to Sji in calc_sji as return value.
subsymbolic, fan(J,F), chunk(I,_), chunk_has_slot(I,_,J) \ calc_sji(J,I,Sji) <=> I \== J | Sji is 2 - log(F).
calc_sji(_,_,Sji) <=> Sji=0.

chunk(Name,Type) \ module_request(goal,chunk(Name,Type,_),_,ResChunk,ResState,RelTime) <=> return_chunk(Name,ResChunk), ResState=free, RelTime=0.
module_request(goal,_,_,ResChunk,ResState,RelTime) <=> ResChunk = nil, ResState=error, RelTime=0. % chunk not found

module_request(retrieval,nil,_,ResChunk,ResState,RelTime) <=> ResChunk = nil,ResState=free,RelTime=1. %TODO: Add proper time (activation)
subsymbolic \ module_request(retrieval,chunk(Name,Type,Slots),Context,ResChunk,ResState,RelTime) <=> 
  find_chunk(Name,Type,Slots),
  collect_matches(Res),
  %write('Matches: '),write(Res),nl,
  calc_activations(Res,Context),
  % find threshold for maximum check
  get_conf(rt,RT),
  %write(rt:RT),nl,
  threshold(RT),
  get_max(MaxChunk,MaxAct),
  return_chunk(MaxChunk,ResChunk),
  get_state(ResChunk,ResState),
  calc_time(MaxAct,RelTime).
  
module_request(retrieval,chunk(Name,Type,Slots),_,ResChunk,ResState,RelTime) <=> 
  find_chunk(Name,Type,Slots),
  collect_matches(Res),
  %write('Matches: '),write(Res),nl,
  first(Res,Chunk),
  return_chunk(Chunk,ResChunk),
  get_state(ResChunk,ResState),
  RelTime=1.

first([],nil).
first([X|_],X).  
  
calc_time(Act,ResTime) :-
  get_conf(lf,F),
  ResTime=F*exp(-Act). 

find_chunk(N1,T1,Ss), chunk(N2,T2) ==> unifiable((N1,T1),(N2,T2),_), nonvar(Ss) | test_slots(N2,Ss), match_set([N2]).
find_chunk(N1,T1,Ss), chunk(N2,T2) ==> unifiable((N1,T1),(N2,T2),_), var(Ss) | test_slots(N2,[]), match_set([N2]).
find_chunk(_,_,_) <=> true.

test_slots(_,[]) <=> true.
chunk_has_slot(N,S,V1), match_set([N]) \ test_slots(N,[(S,V2)|Ss]) <=> unifiable(V1,V2,_) | test_slots(N,Ss).
chunk_has_slot(N,S,V1) \ test_slots(N,[(S,V2)|_]), match_set([N]) <=> \+unifiable(V1,V2,_) | true.
test_slots(N,_) \ match_set([N]) <=> true.

collect_matches(_) \ match_set(L1), match_set(L2) <=> append(L1,L2,L), match_set(L).
collect_matches(Res), match_set(L) <=> Res=L.
collect_matches(Res) <=> Res=[].

get_state(nil,error).
get_state(_,free).

max(_,A1) \ max(_,A2) <=> 
  A1 >= A2 |
  true.
  
get_max(MN,MA), max(N,A), threshold(RT) <=> 
  A >= RT | 
  MN=N,
  MA=A,
  write(max:MN:MA),nl.
  
get_max(MN,MA), max(_,A), threshold(RT) <=> 
  A < RT | 
  MN=nil,
  get_conf(rt,RT), % set activation to threshold if no chunk has activation higher than threshold
  MA=RT,
  write('No chunk has high enough threshold'),nl,
  write(max:MN:MA),nl.
  
get_max(MN,MA), threshold(RT) <=>
  MN=nil,
  % set activation to threshold if no chunk matches
  MA=RT,
  write('No chunk matches.'),nl,
  write(max:MN:MA),nl.


%
% Calculate activation
%

% save presentation time of chunk
identical(C1,C2), present(chunk(C2,_,_)) <=> present(C1). % when chunks have been merged: strengthen the old chunk (since the new chunk is not available any more).
present(chunk(Name,_,_)) <=> get_now(Time),presentation(Name,Time).

:- chr_constraint context/3.

context(_,[],Assoc) <=> Assoc=0.
context(I,[J|Js],Assoc) <=> 
  calc_sji(J,I,Sji),
  write(sji:J:I:Sji),nl,nl,
  context(I,Js,Assoc1), 
  Assoc is Assoc1+Sji.

calc_activations([],_) <=>
  true.
calc_activations([C|Cs],Context) <=> 
  calc_activation(C,B), 
  calc_activations(Cs,Context),
  context(C,Context,Assoc),
  length(Context,N),
  write(context:Context:N),nl,
  Assoc1 is 1/N * Assoc,
  A is B + Assoc1,
  write('Activation Chunk '),
  write(C:A),nl,
  max(C,A).
  
% for each presentation of chunk C, a base level activation part has to be calculated. These parts are put together as a sum for the actual base level activation of chunk C.
presentation(C,PTime), calc_activation(C,A) ==> get_now(Now), Time is Now - PTime, base_level_part(C,Time,_,A).
calc_activation(_,_) <=> true.

% if A and B not set: set B to Time^(-D). Time is the time since the presentation of this base_level_part
base_level_part(_,Time,B,A) ==>
  var(A), var(B), Time =\= 0 |
  get_conf(bll,D), % decay parameter
  B is Time ** (-D).

% if A and B not set and Time is 0: set B to 0 % TODO: really??
base_level_part(_,Time,B,A) ==>
  var(A), var(B), Time =:= 0 |
  B=0.

% collect base level parts and add them together. Only if Bs are set
base_level_part(C,_,B1,A), base_level_part(C,_,B2,A) <=>
  nonvar(B1), nonvar(B2), var(A) |
  B is B1+B2,
  base_level_part(C,_,B,A).
    
% if B is set, A is not set and there are no more base_level_parts of this chunk: calculate actual base level activation and store it in A. Only possible if B is =\= 0.
base_level_part(_,_,B,A) <=>
  var(A),nonvar(B), B =\= 0 |
  A is log(B).
  
% if B is set to 0, A is not set and there are no more base_level_parts of this chunk: set actual base level activation to 0 % TODO really??
base_level_part(_,_,B,A) <=>
  var(A),nonvar(B), B == 0 |
  A is 0.