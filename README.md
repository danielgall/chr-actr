CHR-ACT-R
=========

An Implementation of ACT-R in CHR

See the my master thesis for details of the implementation.

The source of the implementation presented in this work can be found in
the directory src/. It is separated into two parts: the compiler and the
framework.
• The compiler is in the directory compiler. It can be started by consulting
  actr2chr.pl in SWI-Prolog. The query compile_file(f). compiles the
  file f. There are several example model files in the directory of the compiler
  which all start with the prefix example_.
• The framework can be found in the directory core. To load a model, the
  compiled model file has to be consulted in SWI-Prolog. There are several
  compiled example models all starting with the prefix example_. Make sure
  that the models are in the same folder as the framework. The query run. runs
  the model.
