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

:- chr_constraint add_chunk_type(+,+).
% add_chunk_type(ChunkTypeName, [SlotNames]).

%
% Rules
%

add_chunk_type(CT, []) <=> chunk_type(CT).
add_chunk_type(CT, [S|Ss]) <=> chunk_type_has_slot(CT, S), add_chunk_type(CT, Ss).

%add_dm(Chunk , []) <=> chunk(Chunk, Type).
%add_dm(Chunk, [S::=Val|SVals])  <=> chunk_has_slot(S,Val), add_dm(Chunk, SVals).

