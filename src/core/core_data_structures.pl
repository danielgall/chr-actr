% This file has no module. All constraints will be loaded into the local module.
%
:- use_module(library(chr)).

:- chr_type chunk_def ---> nil; chunk(any, any, slot_list).
:- chr_type list(T) ---> []; [T | list(T)].
:- chr_type slot_list == list(pair(any,any)). % a list of slot-value pairs
:- chr_type pair(T1,T2) ---> (T1,T2).

:- chr_type lchunk_defs == list(chunk_def).