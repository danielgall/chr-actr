:- use_module(tokenizer).
:- use_module(parser).

compile(X) :- getTokens(X,T), parse(T,S), write(S).