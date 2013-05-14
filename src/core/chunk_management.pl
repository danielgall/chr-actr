:- use_module(library(chr)).

%%%%%%%%%%%%%%
% Data Types %
%%%%%%%%%%%%%%

:- include(core_data_structures).

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

:- chr_constraint add_chunk(+chunk_def).
% add_chunk(+Chunk)
% Adds a chunk defined by Chunk to the store.
% The argument Chunk must be of type chunk.

:- chr_constraint add_chunks(+lchunk_defs).
% add_chunks(+ListofChunks)
% Adds a chunk defined by Chunk to the store.
% The argument Chunk must be a list of chunk definitions.

:- chr_constraint return_chunk(+,-chunk_def).

%
% private

:- chr_constraint build_chunk_list(+chunk_def,-chunk_def).

%%%%%%%%%
% Rules %
%%%%%%%%%

add_chunk_type(CT, []) <=> chunk_type(CT).
add_chunk_type(CT, [S|Ss]) <=> chunk_type_has_slot(CT, S), add_chunk_type(CT, Ss).

add_chunk(nil) <=> true.
add_chunk(chunk(Name, Type, [])) <=> chunk(Name, Type).
add_chunk(chunk(Name, Type, [(S,V)|Rest]))  <=> chunk_has_slot(Name, S,V), add_chunk(chunk(Name,Type,Rest)).

add_chunks([]) <=> true.
add_chunks([C|Cs]) <=> add_chunk(C), add_chunks(Cs).

chunk(ChunkName, ChunkType) \ return_chunk(ChunkName,Res) <=> var(Res) | build_chunk_list(chunk(ChunkName, ChunkType, []),Res).

chunk_has_slot(ChunkName, S, V) \ build_chunk_list(chunk(ChunkName, ChunkType, L), Res) <=> \+member((S,V),L) | build_chunk_list(chunk(ChunkName, ChunkType, [(S,V)|L]),Res).
build_chunk_list(X,Res) <=> Res=X.