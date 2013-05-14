:- module(buffers, [buffer_request/2, add_buffer/2]).
:- use_module(library(chr)). 

:- use_module(module_handler).

%%%%%%%%%%%%%%
% Data Types %
%%%%%%%%%%%%%%

:- include(core_data_structures).

%%%%%%%%%%%%%%%%%%%%
% Data Constraints %
%%%%%%%%%%%%%%%%%%%%

:- chr_constraint buffer(+,+,+chunk_def).
% buffer(Name, ModName, Value)

:- chr_constraint declarative_module(+). % saves the name of the declarative module (may be exchanged if another implementation should be used)

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

%
% Private

:- chr_constraint write_to_dm(+chunk_def).
% write_to_dm(+Chunk)
% Writes the chunk Chunk to declarative memory.
% The module which acts as declarative memory is defined by declarative_module/1.

%%%%%%%%%
% Rules %
%%%%%%%%%

% Handle add_buffer
buffer(BufName, _, _) \ add_buffer(BufName, _) <=> false. % buffers must have distinct names
add_buffer(BufName, ModName) <=> buffer(BufName, ModName, nil).

% Handle buffer_request
buffer_request(BufName, Chunk), buffer(BufName, ModName, _) <=> do_action(ModName, module_request(BufName, Chunk, ResChunk)), buffer(BufName, ModName, ResChunk).

% Handle buffer_change
buffer_change(BufName, NewChunk), buffer(BufName, ModName, OldChunk) <=> buffer(BufName, ModName, NewChunk). % TODO write OldChunk to dm

% Handle buffer_clear
buffer_clear(BufName), buffer(BufName, ModName, Chunk) <=> write_to_dm(Chunk), buffer(BufName, ModName, nil).

% Handle write_to_dm
declarative_module(DM) \ write_to_dm(Chunk) <=> do_action(DM, add_dm(Chunk)).