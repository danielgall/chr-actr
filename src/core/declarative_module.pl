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

:- chr_constraint find_chunk(+,-).

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

module_request(retrieval,Chunk,ResChunk) <=> find_chunk(Chunk,ResChunk).

chunk(Name, Type) \ find_chunk(chunk(Name,Type,Slots), ResChunk) <=> check_slots(ChunkName, Slots), return_chunk(ChunkName,ResChunk).
%chunk(ChunkName, ChunkType) \ find_chunk(chunk(Name,Type,Slots), ResChunk) <=> Name==ChunkName, Type==ChunkType | check_slots(ChunkName, Slots), return_chunk(ChunkName,ResChunk).

check_slots(_, []) <=> true.
chunk_has_slot(ChunkName, S, V) \ check_slots(ChunkName, [(S,V)|Rest]) <=> check_slots(ChunkName, Rest).
check_slots(_, _) <=> false.
