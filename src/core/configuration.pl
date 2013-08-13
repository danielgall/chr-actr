:- module(configuration,[get_conf/2,set_conf/2,add_config_observer/2]).
:- use_module(library(chr)).

:- chr_constraint get_conf/2, set_conf/2, configuration/2, add_config_observer/2, observer/2,notify/2,notify_all/1.


% Implements interface observable
%
% methods:
% add_config_observer(Module,Variable)
% Module: the module that wants to observe a variable
% Variable: the variable which should be observed.
% If a config variable that is observed is set, all observers are notified by calling their update constraint.

% special rules:

latency-factor-by-threshold @
set_conf(rt,RT) ==> LF is 0.35*exp(RT), set_conf(lf,LF).

% error detection rules

% only t or nil for esc allowed
set_conf(esc,X) <=> X \== t, X \== nil | write('ERROR: please set esc only to t or nil!'),nl,false.

% general rules

configuration(Var,Val) \ get_conf(Var,Res) <=> Res = Val.

observer(Module,Var), set_conf(Var,_) ==> notify(Module,Var). % collect all modules that want to be notified

set_conf(Var,Val), configuration(Var,_) <=> 
  configuration(Var,Val),
  notify_all(Var). % variable has been set -> perform pending notifications

set_conf(Var,Val) <=> 
  configuration(Var,Val),
  notify_all(Var). % variable has been set -> perform pending notifications

notify_all(Var), notify(Module,Var) ==> Module:update. % perform all pending notifications
notify_all(_) <=> true. % no notifications pending -> Clean up

% default values (if no values have been set)
get_conf(rt,_) ==> set_conf(rt,-0.5).
get_conf(bll,_) ==> set_conf(bll,0.5).
get_conf(alpha,_) ==> set_conf(alpha,0.2).
get_conf(esc,_) ==> set_conf(esc,nil).


% observer stuff

add_config_observer(Module,Variable) <=>
  observer(Module,Variable).
