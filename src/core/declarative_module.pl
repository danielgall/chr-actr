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

:- chr_constraint collect_matches(-).
% collects all the elements in a match_set constraint

:- chr_constraint test_slots(+,+slot_list).

:- chr_constraint match_set(+list(any)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implemented abstract constraints from interface "module" %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- chr_constraint module_request(+,+chunk_def,+chunk_def).
% module_request(BufName,Chunk,ResChunk)

% add_chunk_type(ChunkTypeName, [SlotNames]).

%%%%%%%%%
% Rules %
%%%%%%%%%

add_dm(ChunkDef) <=> add_chunk(ChunkDef).


chunk(Name,Type) \ module_request(goal,chunk(Name,Type,_),ResChunk) <=> return_chunk(Name,ResChunk).
module_request(goal,_,ResChunk) <=> ResChunk = nil. % chunk not found

module_request(retrieval,nil,ResChunk) <=> ResChunk = nil.
module_request(retrieval,chunk(Name,Type,Slots),ResChunk) <=> 
  find_chunk(Name,Type,Slots),
  collect_matches(Res),
  choose_chunk(Res,ResChunk).

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

choose_chunk([],nil).
choose_chunk([N|_],C):- return_chunk(N,C).