:- module(configuration,[get_conf/2,set_conf/2]).
:- use_module(library(chr)).

:- chr_constraint get_conf/2, set_conf/2, configuration/2.

% special rules:

latency-factor-by-threshold @
set_conf(rt,RT) ==> LF is 0.35*exp(RT), set_conf(lf,LF).

% general rules

configuration(Var,Val) \ get_conf(Var,Res) <=> Res = Val.

set_conf(Var,Val), configuration(Var,_) <=> configuration(Var,Val),write(config-set-to:Var:Val),nl.

set_conf(Var,Val) <=> configuration(Var,Val),write(config-set-to:Var:Val),nl.
