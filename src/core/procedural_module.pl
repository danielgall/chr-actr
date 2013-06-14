:- module(procedural_module, []).
:- use_module(library(chr)).

:- include('buffers.pl').

:- chr_constraint start/0.

:- chr_constraint match/0, conflict_resolution/0, execute/0.

start <=> 
    
    match. % Go to match phase

%
% match phase
%


%
% conflict_resolution phase
%

match <=> conflict_resolution. % match phase is over, go to conflict_resolution phase




%
% execute phase
%

conflict_resolution <=> execute. % conflict_resolution is over, got to execute phase


%
% start new cycle
%

execute <=> match.