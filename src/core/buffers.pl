:- use_module(library(chr)). 

%%%%%%%%%%%%%%
% Data Types %
%%%%%%%%%%%%%%

%:- include(core_data_structures).

%%%%%%%%%%%%%%%%%%%%
% Data Constraints %
%%%%%%%%%%%%%%%%%%%%

:- include(chunk_management).

:- chr_constraint buffer(+,+,+).
% buffer(Name, ModName, ChunkName)

%%%%%%%%%%%%%%%%%%%%%%%%%%
% Procedural Constraints %
%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Public

:- chr_constraint add_buffer(+,+).
% add_buffer(+BufferName, +ModuleName)
% adds a new buffer with name BufferName for module ModuleName. 
% Buffers must have distinct names!
% A module can have multiple buffers.

:- chr_constraint buffer_request(+,+chunk_def).
% buffer_request(+BufferName, +Chunk)
% Performs a module_request on the corresponding module of buffer BufName with the (possibly not fully instantiated) chunk Chunk.

:- chr_constraint buffer_change(+,+chunk_def).
% buffer_change(+BufferName, +NewChunk)
% Changes the value of buffer BufferName to NewChunk.

:- chr_constraint buffer_clear(+).
% buffer_clear(+BufferName)
% Clears the buffer BufferName. This causes the system to write the stored chunk to the declarative memory. The module, which handles the declarative memory actions is defined by declarative_module/1.

% Private

:- chr_constraint set_buffer(+,+chunk_def).
%set_buffer(BufName,Chunk)
% for internal use only!
% use buffer_change/2 instead!
% saves the name of the chunk in Chunk to the buffer without any side-effects.
% Neither is the old chunk saved, nor is a new chunk-constraint with corresponding slots created!

:- chr_constraint delete_chunk(+).

:- chr_constraint write_to_dm(+).
% write_to_dm(+Chunk)
% Writes the chunk Chunk to declarative memory.
% The module which acts as declarative memory is defined by declarative_module/1.

%%%%%%%%%
% Rules %
%%%%%%%%%

:- chr_constraint do_buffer_request/2,do_buffer_change/2,do_buffer_clear/1, buffer_state/2, set_buffer_state/2, performed_request/3.

% write error state
buffer_state(BufName,error) ==> write('error in buffer '),write(BufName),nl.

% Handle add_buffer
buffer(BufName, _, _) \ add_buffer(BufName, _) <=> false. % buffers must have distinct names

% create empty buffer
add_buffer(BufName, ModName) <=> 
buffer(BufName, ModName, nil),
buffer_state(BufName,free). 

% Schedule buffer_request
buffer(BufName, ModName, _) \ buffer_request(BufName, Chunk) <=> %% todo: check for free buffer!!
  get_now(Now),
  buffer_state(BufName,busy),
  do_buffer_clear(BufName), % clear buffer immediately
  get_context(Context),
  ModName:module_request(BufName, Chunk, Context, ResChunk,ResState,RelTime),
  performed_request(BufName, ResChunk, ResState), % save result of request
  Time is Now + RelTime, 
  add_q(Time, 0, do_buffer_request(BufName, Chunk)).

  % TODO Buffer states noch nicht korrekt?? Tests…
  
% Handle buffer_request
do_buffer_request(BufName, Chunk), buffer(BufName, ModName, _), buffer_state(BufName,_), performed_request(BufName, ResChunk, ResState) <=>  %% todo: check for free buffer!!
  write('performing request: '),write(BufName),nl,
  (ResState=error, 
  buffer(BufName, ModName, nil),
  buffer_state(BufName,error) ;
  
  ResState = free,
  ResChunk = chunk(ResChunkName,_,_),
  add_chunk(ResChunk), 
  buffer(BufName, ModName, ResChunkName),
  buffer_state(BufName,free)).
  
% Schedule buffer_change
buffer_change(BufName, Chunk) <=> 
  get_now(Now),
  write(now:Now),nl,
  Time is Now + 0, 
  add_q(Time, 100, do_buffer_change(BufName, Chunk)).

% Handle buffer_change
buffer(BufName, _, OldChunk) \ do_buffer_change(BufName, chunk(_,_,SVs)) <=>
  alter_slots(OldChunk,SVs).

  
set_buffer(BufName, nil), buffer(BufName, ModName, _)  <=> 
  buffer(BufName, ModName, nil).
  
set_buffer(BufName, chunk(ChunkName, _, _)), buffer(BufName, ModName, _) <=>
  buffer(BufName, ModName, ChunkName).
  
set_buffer_state(BufName, State), buffer_state(BufName, _) <=> buffer_state(BufName, State).
  

% Schedule buffer_clear
buffer_clear(BufName) <=> 
  get_now(Now),
  Time is Now + 0, 
  add_q(Time, 10, do_buffer_clear(BufName)).  
  
% Handle buffer_clear
do_buffer_clear(BufName), buffer(BufName, ModName, Chunk) <=> 
  write('clear buffer '),write(BufName),nl, 
  write_to_dm(Chunk), 
  delete_chunk(Chunk), 
  buffer(BufName, ModName, nil).

delete_chunk(Name) \ chunk(Name,_) <=> true.
delete_chunk(Name) \ chunk_has_slot(Name,_,_) <=> true.
delete_chunk(_) <=> true.

% Handle write_to_dm
write_to_dm(ChunkName) <=> return_chunk(ChunkName, ResChunk), add_dm(ResChunk).