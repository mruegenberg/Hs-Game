import Numeric.LinearProgramming

prob = Maximize [4, -3, 2]

constr1 = Sparse [ [2#1, 1#2] :<: 10
                 , [1#2, 5#3] :<: 20
                 ]
          
foo = simplex prob constr1 []