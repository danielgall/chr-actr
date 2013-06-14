% Implements interface "module"
:- module(declarative_module, [add_dm/1]).
:- use_module(library(chr)).

:- include('chunk_management.pl').

%%%%%%%%%%%%%%
% Data Types %
%%%%%%%%%%%%%%

% derived from chunk_management

%%%%%%%%%%%%%%%%%%%%
% Data Constraints %
%%%%%%%%%%%%%%%%%%%%

% derived from chunk_management


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

:- chr_constraint module_request(+,+chunk_def,-chunk_def,-,-).
% module_request(BufName,Chunk,ResChunk,ResState,RelTime)

% add_chunk_type(ChunkTypeName, [SlotNames]).

%%%%%%%%%
% Rules %
%%%%%%%%%

add_dm(ChunkDef) <=> add_chunk(ChunkDef).


chunk(Name,Type) \ module_request(goal,chunk(Name,Type,_),ResChunk,ResState,RelTime) <=> return_chunk(Name,ResChunk), ResState=free, RelTime=0.
module_request(goal,_,ResChunk,ResState,RelTime) <=> ResChunk = nil, ResState=error, RelTime=0. % chunk not found

module_request(retrieval,nil,ResChunk,ResState,RelTime) <=> ResChunk = nil,ResState=free,RelTime=1. %TODO: Add proper time (activation)
module_request(retrieval,chunk(Name,Type,Slots),ResChunk,ResState,RelTime) <=> 
  find_chunk(Name,Type,Slots),
  collect_matches(Res),
  %write('Matches: '),write(Res),nl,
  choose_chunk(Res,ResChunk,ResState),
  RelTime=1.

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

choose_chunk([],nil,error).
choose_chunk([N|_],C,free):- return_chunk(N,C).