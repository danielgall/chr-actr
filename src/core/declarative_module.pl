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

:- chr_constraint base_level_part/4, calc_activation/2, calc_activations/2, max/2, get_max/2.

:- chr_constraint present(+chunk_def).

:- chr_constraint presentation(+,+).
% presentation(Chunk, Time)


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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implemented abstract constraints from interface "module" %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- chr_constraint module_request(+,+chunk_def,+,-chunk_def,-,-).
% module_request(BufName,Chunk,ContextResChunk,ResState,RelTime)

% add_chunk_type(ChunkTypeName, [SlotNames]).

%%%%%%%%%
% Rules %
%%%%%%%%%

%%% IMPORTANT TODO: ADD CHUNK MERGING!!!!!!!!!!!!! %%%%%

add_dm(ChunkDef) <=> add_chunk(ChunkDef), present(ChunkDef).

% Calculate Fan of each chunk
:- chr_constraint fan/2, sji/2.

chunk(C,_) ==> fan(C,1).
chunk_has_slot(_,_,C), chunk(C,_) ==> fan(C,1).

fan(C,F1), fan(C,F2) <=> F is F1+F2, fan(C,F).

% Calculate S_ji
fan(C,F) ==> Sji is 2 - log(F), sji(C,Sji).

chunk(Name,Type) \ module_request(goal,chunk(Name,Type,_),_,ResChunk,ResState,RelTime) <=> return_chunk(Name,ResChunk), ResState=free, RelTime=0.
module_request(goal,_,_,ResChunk,ResState,RelTime) <=> ResChunk = nil, ResState=error, RelTime=0. % chunk not found

module_request(retrieval,nil,_,ResChunk,ResState,RelTime) <=> ResChunk = nil,ResState=free,RelTime=1. %TODO: Add proper time (activation)
module_request(retrieval,chunk(Name,Type,Slots),Context,ResChunk,ResState,RelTime) <=> 
  find_chunk(Name,Type,Slots),
  collect_matches(Res),
  write('Matches: '),write(Res),nl,
  calc_activations(Res,Context),
  % find threshold for maximum check
  get_conf(rt,RT),
  write(rt:RT),nl,
  threshold(RT),
  get_max(MaxChunk,MaxAct),
  return_chunk(MaxChunk,ResChunk),
  get_state(ResChunk,ResState),
  calc_time(MaxAct,RelTime).
  
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
present(chunk(Name,_,_)) <=> get_now(Time),presentation(Name,Time).

:- chr_constraint context/2.

context([],Assoc) <=> Assoc=0.
sji(C,Sji) \ context([C|Cs],Assoc) <=> context(Cs,Assoc1), write(c:C:cs:Cs:assoc1:Assoc1),nl,Assoc is Assoc1+Sji,write(assoc:Assoc),nl.

calc_activations([],_) <=>
  true.
calc_activations([C|Cs],Context) <=> 
  calc_activation(C,B), 
  calc_activations(Cs,Context),
  context(Context,Assoc),
  length(Context,N),
  write(context:Context:N),nl,
  Assoc1 is 1/N * Assoc,
  A is B + Assoc1,
  write('Activation Chunk '),
  write(C:A),nl,
  max(C,A).
  
presentation(C,PTime), calc_activation(C,A) ==> get_now(Now), Time is Now - PTime, base_level_part(C,Time,_,A).
calc_activation(_,_) <=> true.

base_level_part(_,Time,B,A) ==>
  var(A), var(B), Time =\= 0 |
  write('komisch'),nl,
  get_conf(bll,D), % decay parameter
  B is Time ** (-D).
  
base_level_part(_,Time,B,A) ==>
  var(A), var(B), Time =:= 0 |
  write('komisch1'),nl,
  B=0.

base_level_part(C,_,B1,A), base_level_part(C,_,B2,A) <=>
  nonvar(B1), nonvar(B2), var(A) |
  B is B1+B2,
  base_level_part(C,_,B,A).
    
  
base_level_part(_,_,B,A) <=>
  var(A),nonvar(B), B =\= 0 |
  A is log(B).

base_level_part(_,_,B,A) <=>
  var(A),nonvar(B), B == 0 |
  write('somehow B is 0...'),nl,
  A is 0.