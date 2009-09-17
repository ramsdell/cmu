This package contains a library for unification in
commutative monoid and a program that exercises the library.

$ cmu
Commutative monoid unification -- :? for help
cmu> 2x+y=3z
Problem:   2x + y = 3z
Unifier:   [x : g0 + 3g2,y : g0 + 3g1,z : g0 + g1 + 2g2]

cmu> 2x=x+y
Problem:   2x = x + y
Unifier:   [x : g0,y : g0]

cmu> 64x=41y+a
Problem:   64x = a + 41y
Unifier:   [a : 5g0 + 2g1 + 23g2 + g3 + 64g4,x : 2g0 + 9g1 + g2 + 25g3 + g4 + 41g5,y : 3g0 + 14g1 + g2 + 39g3 + 64g5]

cmu> :quit
