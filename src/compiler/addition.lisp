(p initialize-addition
   =goal>
      isa         add
      arg1        =num1
      arg2        =num2
      sum         nil
==>
   =goal>
      sum         =num1
      count       0
   +retrieval>
      isa        count-order
      first      =num1
)

(p terminate-addition
   =goal>
      isa         add
      count       =num
      arg2        =num
      sum         =answer
==>
   =goal>
      count       nil
)

(p increment-count
   =goal>
      isa         add
      sum         =sum
      count       =count
   =retrieval>
      isa         count-order
      first       =count
      second      =newcount
==>
   =goal>
      count       =newcount
   +retrieval>
      isa        count-order
      first      =sum
)

(p increment-sum
   =goal>
      isa         add
      sum         =sum
      count       =count
    - arg2        =count
   =retrieval>
      isa         count-order
      first       =sum
      second      =newsum
==>
   =goal>
      sum         =newsum
   +retrieval>
      isa        count-order
      first      =count
)