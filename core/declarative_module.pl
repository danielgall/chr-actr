% Implements interface "module"
:- module(declarative_module, [module_request/3]).
:- use_module(library(chr)).

%%%%%%%%%%%%%%%%%%%%
% Data Constraints %
%%%%%%%%%%%%%%%%%%%%
:- chr_constraint chunk(+,+).
% chunk(ChunkName, ChunkType)
% 

:- chr_constraint chunk_type(+).

:- chr_constraint chunk_type_has_slot(+,+).
% chunk_type_has_slot(ChunkTypeName, SlotName).

:- chr_constraint chunk_has_slot(+,+,+).
% chunk_has_slot(ChunkName, SlotName, Value)


%%%%%%%%%%%%%%%%%%%%%%%%%%
% Procedural Constraints %
%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% public

:- chr_constraint add_chunk_type(+,+).
% add_chunk_type(+ChunkTypeName, +Slots)
% Adds a new chunk type with name ChunkTypeName to the system. The slots are defined in the list Slots.

:- chr_constraint add_dm(+).
% add_dm(+Chunk)
% Adds a chunk defined by Chunk to declarative memory.
% The argument Chunk must be of type chunk.

%
% private
:- chr_constraint check_slots(+,+).

:- chr_constraint find_chunk(+,-).

:- chr_constraint return_chunk(+,-).
:- chr_constraint build_chunk_list(+,-).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implemented abstract constraints from interface "module" %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- chr_constraint module_request(+,+,+).
% module_request(BufName,Chunk,ResChunk)

% add_chunk_type(ChunkTypeName, [SlotNames]).

%%%%%%%%%
% Rules %
%%%%%%%%%

add_chunk_type(CT, []) <=> chunk_type(CT).
add_chunk_type(CT, [S|Ss]) <=> chunk_type_has_slot(CT, S), add_chunk_type(CT, Ss).

add_dm(chunk(Name, Type, [])) <=> chunk(Name, Type).
add_dm(chunk(Name, Type, [(S,V)|Rest]))  <=> chunk_has_slot(Name, S,V), add_dm(chunk(Name,Type,Rest)).

module_request(_,Chunk,ResChunk) <=> find_chunk(Chunk,ResChunk).

chunk(ChunkName, ChunkType) \ find_chunk(chunk(Name,Type,Slots), ResChunk) <=> Name=ChunkName,Type=ChunkType,check_slots(ChunkName, Slots), return_chunk(ChunkName,ResChunk).
%chunk(ChunkName, ChunkType) \ find_chunk(chunk(Name,Type,Slots), ResChunk) <=> Name==ChunkName, Type==ChunkType | check_slots(ChunkName, Slots), return_chunk(ChunkName,ResChunk).

check_slots(_, []) <=> true.
chunk_has_slot(ChunkName, S, V) \ check_slots(ChunkName, [(S,V)|Rest]) <=> check_slots(ChunkName, Rest).
check_slots(_, _) <=> false.

chunk(ChunkName, ChunkType) \ return_chunk(ChunkName,Res) <=> var(Res) | build_chunk_list(chunk(ChunkName, ChunkType, []),Res).

chunk_has_slot(ChunkName, S, V) \ build_chunk_list(chunk(ChunkName, ChunkType, L), Res) <=> \+member((S,V),L) | build_chunk_list(chunk(ChunkName, ChunkType, [(S,V)|L]),Res).
build_chunk_list(X,Res) <=> Res=X.
