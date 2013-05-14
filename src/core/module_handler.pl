:- module(module_handler, [add_module/1, do_action/2]).
:- use_module(library(chr)).

:- chr_constraint add_module(+).
:- chr_constraint do_action(+,+).

add_module(Module) <=> use_module(Module:Module).

do_action(Module, Action) <=> call(Module:Action).
