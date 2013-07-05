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

:- chr_constraint alter_slots(+,+slot_list), alter_slot(+,+,+).  

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

:- chr_constraint remove_chunk(+).
% remove_chunk(ChunkName)
% removes the chunk with name ChunkName from the store.

:- chr_constraint return_chunk(+,-chunk_def).

%
% private

:- chr_constraint build_chunk_list(+chunk_def,-chunk_def).

:- chr_constraint do_add_chunk(+chunk_def).

%%%%%%%%%
% Rules %
%%%%%%%%%

add_chunk_type(CT, []) <=> chunk_type(CT).
add_chunk_type(CT, [S|Ss]) <=> chunk_type_has_slot(CT, S), add_chunk_type(CT, Ss).

% 
% Adding of multiple chunks at once
add_chunks([]) <=> true.
add_chunks([C|Cs]) <=> add_chunk(C), add_chunks(Cs).

%
% Add a chunk to memory

add_chunk(chunk(Name,_,_)) \ chunk(Name,Type) <=> % delete chunk of Type chunk, if real chunk is added
  Type == chunk |
  true.

chunk(Name,Type) \ add_chunk(chunk(Name,Type,Slots)) <=>
  Type \== chunk |
  add_chunk(chunk(Name:new,Type,Slots)).

reduce @ identical(C1,C2) \ identical(C2,C3) <=> identical(C1,C3).
  
identical(_,C) \ add_chunk(chunk(C,_,_)) <=> true.

% delete chunks with new name
identical(C,C:new) <=> true.

% empty chunk will not be added
add_chunk(nil) <=> true.

% first check, if identical chunk exists
add_chunk(chunk(C,T,S)) ==> 
  T \== chunk | % do not check chunks of special type chunk, which has no slots. %% TODO: maybe dont check chunks with no slots generally
  check_identical_chunks(chunk(C,T,S)).
  
% initialize all slots with nil. This leads to complete chunk definitions in store. Values that are not set stay nil.
add_chunk(chunk(Name,Type, _)), chunk_type_has_slot(Type,S) ==> 
  chunk_has_slot(Name,S,nil).

% chunk has been initialized with empty slots -> actually add chunk
add_chunk(chunk(Name,Type, Slots)) <=>
  do_add_chunk(chunk(Name,Type,Slots)).

do_add_chunk(chunk(Name, Type, [])) <=> chunk(Name, Type). % base case

chunk(V,_) \ do_add_chunk(chunk(Name, Type, [(S,V)|Rest])), chunk_has_slot(Name,S,nil)  <=> % overwrite slots with empty values
  chunk_has_slot(Name, S,V), 
  do_add_chunk(chunk(Name,Type,Rest)).
  
do_add_chunk(chunk(Name, Type, [(S,V)|Rest])), chunk_has_slot(Name,S,nil)  <=> % overwrite slots with empty values
  V == nil | % do not add chunk(nil,chunk)
  chunk_has_slot(Name, S,V), 
  do_add_chunk(chunk(Name,Type,Rest)).  
  
do_add_chunk(chunk(Name, Type, [(S,V)|Rest])), chunk_has_slot(Name,S,nil)  <=> % overwrite slots with empty values
  V \== nil |
  chunk_has_slot(Name, S,V), 
  chunk(V,chunk), % no chunk for slot value found => add chunk of type chunk
  do_add_chunk(chunk(Name,Type,Rest)).

%
% Chunk merging (needed for adding)

:- chr_constraint check_identical_chunks/1, check_identical_chunks/2, identical/2.

% Merge chunks with different names
check_identical_chunks(nil) <=> true.
chunk(NameOld,Type), check_identical_chunks(chunk(NameNew,Type,Slots)) ==> 
  check_identical_chunks(chunk(NameNew,Type,Slots),NameOld).
check_identical_chunks(_) <=> true.

chunk(NameOld, Type) \ check_identical_chunks(chunk(NameNew,Type,[]),NameOld) <=> 
  identical(NameOld,NameNew).
chunk(NameOld, Type), chunk_has_slot(NameOld, S, V) \ check_identical_chunks(chunk(NameNew,Type,[(S,V)|Rest]),NameOld) <=> 
  check_identical_chunks(chunk(NameNew,Type,Rest),NameOld).
chunk(NameOld, Type), chunk_has_slot(NameOld, S, VOld) \ check_identical_chunks(chunk(_,Type,[(S,VNew)|_]),NameOld) <=> 
  VOld \== VNew |
  true.
  
  
remove_duplicates @ identical(N,N) <=> true.

% abort checking for identical chunks if one has been found
cleanup_identical_chunk_check @ identical(NameOld,NameNew) \ check_identical_chunks(chunk(NameNew,_,_),NameOld) <=> true.


%
% Private operations

alter_slots(_,[]) <=> true.
alter_slots(Chunk,[(S,V)|SVs]) <=> 
  alter_slot(Chunk,S,V),
  alter_slots(Chunk,SVs).
  
alter_slot(Chunk,Slot,Value), chunk_has_slot(Chunk,Slot,_) <=>
  chunk_has_slot(Chunk,Slot,Value).
  
alter_slot(Chunk,Slot,Value) <=>
  chunk_has_slot(Chunk,Slot,Value).

remove_chunk(Name) \ chunk(Name, _) <=> true.
remove_chunk(Name) \ chunk_has_slot(Name, _, _) <=> true.
remove_chunk(_) <=> true.

chunk(ChunkName, ChunkType) \ return_chunk(ChunkName,Res) <=> var(Res) | build_chunk_list(chunk(ChunkName, ChunkType, []),Res).

chunk_has_slot(ChunkName, S, V) \ build_chunk_list(chunk(ChunkName, ChunkType, L), Res) <=> \+member((S,V),L) | build_chunk_list(chunk(ChunkName, ChunkType, [(S,V)|L]),Res).
build_chunk_list(X,Res) <=> Res=X.

name(chunk(Name,_,_),Name).
type(chunk(_,Type,_), Type).
slots(chunk(_,_,Slots),Slots).