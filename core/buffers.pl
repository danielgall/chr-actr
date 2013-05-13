:- module(buffers, [buffer_request/2]).
:- use_module(library(chr)). 

:- use_module(module_handler).

:- chr_constraint buffer(+,+,+).

% buffer(Name, ModName, Value)

:- chr_constraint buffer_request(+,+).

buffer_request(BufName, Chunk), buffer(BufName, ModName, _) <=> do_action(ModName, module_request(BufName, Chunk, ResChunk)), buffer(BufName, ModName, ResChunk).
