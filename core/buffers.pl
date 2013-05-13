:- module(buffers, [buffer_request/2, add_buffer/2]).
:- use_module(library(chr)). 

:- use_module(module_handler).

:- chr_constraint buffer(+,+,+).
% buffer(Name, ModName, Value)

:- chr_constraint add_buffer(+,+).

:- chr_constraint buffer_request(+,+).

:- chr_constraint buffer_change(+,+).

:- chr_constraint buffer_clear(+).

:- chr_constraint declarative_module(+). % saves the name of the declarative module (may be exchanged if another implementation should be used)

:- chr_constraint write_to_dm(+).

buffer_request(BufName, Chunk), buffer(BufName, ModName, _) <=> do_action(ModName, module_request(BufName, Chunk, ResChunk)), buffer(BufName, ModName, ResChunk).

buffer(BufName, _, _) \ add_buffer(BufName, _) <=> false. % buffers must have distinct names
add_buffer(BufName, ModName) <=> buffer(BufName, ModName, nil).

buffer_change(BufName, NewChunk), buffer(BufName, ModName, OldChunk) <=> buffer(BufName, ModName, NewChunk). % TODO write OldChunk to dm

buffer_clear(BufName), buffer(BufName, ModName, Chunk) <=> write_to_dm(Chunk), buffer(BufName, ModName, nil).

declarative_module(DM) \ write_to_dm(Chunk) <=> do_action(DM, add_dm(Chunk)).