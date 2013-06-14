% :- include('procedural_module.pl').
:- use_module(library(chr)).

% translation of
% (P counting example
    %=goal>
      %isa	count
      %state 	incrementing
      %number	=num1
    %=retrieval>
      %isa	count-order
      %first	=num1
      %second	=num2
  %==>
    %=goal>
      %number	=num2
    %+retrieval>
      %isa	count-order
      %first	=num2
%)

%counting_example @ proc <=> buffer_matches(goal, chunk(_,count,[(state,incrementing),(number,Num1)])), buffer_matches(retrieval, chunk(_,count_order,[(first,Num1),(second,Num2)])) <=> set_buffer

counting_example @ buffer(goal,Chunk1), chunk(Chunk1,count), chunk_has_slot(Chunk1,state,incrementing), chunk_has_slot(Chunk1,number,Num1),
		   buffer(retrieval,Chunk2), chunk(Chunk2,count_order), chunk_has_slot(Chunk2,first,Num1), chunk_has_slot(second,Num2) 
		   ==> 
		   set_buffer(goal,chunk(,_,[number,Num2])), request_buffer( usw.
		   
	