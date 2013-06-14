:- module(test_module, []).
:- use_module(library(chr)).

:- chr_constraint module_request/3, heyho/0.

module_request(_,_,_) <=> heyho.