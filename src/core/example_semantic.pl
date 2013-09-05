:- include('actr_core.pl').
:- chr_constraint run/0, fire/0.


delay-initial-retrieve @
  fire,
  buffer(goal,_,A),
    chunk(A,is-member),
      chunk_has_slot(A,object,B),
      chunk_has_slot(A,category,C),
      chunk_has_slot(A,judgment,nil)
==>
  B\==nil,
  C\==nil |
  conflict_set(initial-retrieve).
  
initial-retrieve @
  buffer(goal,_,A),
    chunk(A,is-member),
      chunk_has_slot(A,object,B),
      chunk_has_slot(A,category,C),
      chunk_has_slot(A,judgment,nil)
  \ apply_rule(initial-retrieve)
<=>
  B\==nil,
  C\==nil |
  buffer_change(goal,
    chunk(_,_,[ (judgment,pending)])),
  buffer_request(retrieval,
    chunk(_,property,
      [ (object,B), 
        (attribute,category)])),
  conflict_resolution.
  
delay-direct-verify @
  fire,
  buffer(goal,_,A),
    chunk(A,is-member),
      chunk_has_slot(A,object,C),
      chunk_has_slot(A,category,D),
      chunk_has_slot(A,judgment,pending),
  buffer(retrieval,_,B),
    chunk(B,property),
      chunk_has_slot(B,object,C),
      chunk_has_slot(B,attribute,category),
      chunk_has_slot(B,value,D)
==>
  C\==nil,
  D\==nil |
  conflict_set(direct-verify).
  
direct-verify @ 
  buffer(goal,_,A),
    chunk(A,is-member),
      chunk_has_slot(A,object,C),
      chunk_has_slot(A,category,D),
      chunk_has_slot(A,judgment,pending),
  buffer(retrieval,_,B),
    chunk(B,property),
      chunk_has_slot(B,object,C),
      chunk_has_slot(B,attribute,category),
      chunk_has_slot(B,value,D)
  \ apply_rule(direct-verify)
<=>
  C\==nil,
  D\==nil |
  buffer_change(goal,
    chunk(_,_,[ (judgment,yes)])),
  conflict_resolution.
  
delay-chain-category @ 
  fire,
  buffer(goal,_,A),
    chunk(A,is-member),
      chunk_has_slot(A,object,C),
      chunk_has_slot(A,category,D),
      chunk_has_slot(A,judgment,pending),
  buffer(retrieval,_,B),
    chunk(B,property),
      chunk_has_slot(B,object,C),
      chunk_has_slot(B,attribute,category),
      chunk_has_slot(B,value,E)
==> % duplicate slot test has been removed
  C\==nil,
  D\==nil,
  E\==nil,
  E\==D |
  conflict_set(chain-category).
chain-category@
  buffer(goal,_,A),
    chunk(A,is-member),
      chunk_has_slot(A,object,C),
      chunk_has_slot(A,category,D),
      chunk_has_slot(A,judgment,pending),
  buffer(retrieval,_,B),
    chunk(B,property),
      chunk_has_slot(B,object,C),
      chunk_has_slot(B,attribute,category),
      chunk_has_slot(B,value,E)
  \ apply_rule(chain-category)
<=>
  C\==nil,
  D\==nil,
  E\==nil,
  E\==D |
  buffer_change(goal,
    chunk(_,_,[ (object,E)])),
  buffer_request(retrieval,
    chunk(_,property,
      [ (object,E), 
        (attribute,category)])),
  conflict_resolution.

delay-fail @
  fire,
  buffer(goal,_,A),
    chunk(A,is-member),
      chunk_has_slot(A,object,B),
      chunk_has_slot(A,category,C),
      chunk_has_slot(A,judgment,pending),
  buffer_state(retrieval,error)
==>
  B\==nil,
  C\==nil |
  conflict_set(fail).
  
fail @
  buffer(goal,_,A),
    chunk(A,is-member),
      chunk_has_slot(A,object,B),
      chunk_has_slot(A,category,C),
      chunk_has_slot(A,judgment,pending),
  buffer_state(retrieval,error)
  \ apply_rule(fail)
<=>
  B\==nil,
  C\==nil |
  buffer_change(goal,
    chunk(_,_,[ (judgment,no)])),
  conflict_resolution.
  
init @
  run <=>
    set_default_utilities([fail,chain-category,direct-verify,initial-retrieve]),
    add_buffer(retrieval,declarative_module),
    add_buffer(goal,declarative_module),
    lisp_chunktype([chunk]),
    lisp_chunktype([property,object,attribute,value]),
    lisp_chunktype([is-member,object,category,judgment]),
    lisp_adddm([
      [shark,isa,chunk],
      [dangerous,isa,chunk],
      [locomotion,isa,chunk],
      [swimming,isa,chunk],
      [fish,isa,chunk],
      [salmon,isa,chunk],
      [edible,isa,chunk],
      [breathe,isa,chunk],
      [gills,isa,chunk],
      [animal,isa,chunk],
      [moves,isa,chunk],
      [skin,isa,chunk],
      [canary,isa,chunk],
      [color,isa,chunk],
      [sings,isa,chunk],
      [bird,isa,chunk],
      [ostrich,isa,chunk],
      [flies,isa,chunk],
      [height,isa,chunk],
      [tall,isa,chunk],
      [wings,isa,chunk],
      [flying,isa,chunk],
      [true,isa,chunk],
      [false,isa,chunk],
      [p1,isa,property,object,shark,attribute,dangerous,value,true],
      [p2,isa,property,object,shark,attribute,locomotion,value,swimming],
      [p3,isa,property,object,shark,attribute,category,value,fish],
      [p4,isa,property,object,salmon,attribute,edible,value,true],
      [p5,isa,property,object,salmon,attribute,locomotion,value,swimming],
      [p6,isa,property,object,salmon,attribute,category,value,fish],
      [p7,isa,property,object,fish,attribute,breathe,value,gills],
      [p8,isa,property,object,fish,attribute,locomotion,value,swimming],
      [p9,isa,property,object,fish,attribute,category,value,animal],
      [p10,isa,property,object,animal,attribute,moves,value,true],
      [p11,isa,property,object,animal,attribute,skin,value,true],
      [p12,isa,property,object,canary,attribute,color,value,yellow],
      [p13,isa,property,object,canary,attribute,sings,value,true],
      [p14,isa,property,object,canary,attribute,category,value,bird],
      [p15,isa,property,object,ostrich,attribute,flies,value,false],
      [p16,isa,property,object,ostrich,attribute,height,value,tall],
      [p17,isa,property,object,ostrich,attribute,category,value,bird],
      [p18,isa,property,object,bird,attribute,wings,value,true],
      [p19,isa,property,object,bird,attribute,locomotion,value,flying],
      [p20,isa,property,object,bird,attribute,category,value,animal],
      [g1,isa,is-member,object,canary,category,bird,judgment,nil],
      [g2,isa,is-member,object,canary,category,animal,judgment,nil],
      [g3,isa,is-member,object,canary,category,fish,judgment,nil]]),
    lisp_goalfocus([g1]), % choose one of g1, g2, g3
    now(0),
    conflict_resolution,nextcyc.

no-rule @
  fire <=>
    conflict_set([]),
    choose.