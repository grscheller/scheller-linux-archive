## Compare implementations for multiplication (Exercise 4).
Compare laziness of the implementations
```
   x * Zero = Zero
   x * S y  = x + x * y
```
verses
```
   x * Zero = Zero
   x * S y  = x * y + x
```
### The "Natural" definition of `(*)`.
The above are minimalist definitions set up to prove theorems about natural
numbers, typically using mathematical induction.  One proves theorems such
as `(*)` is commutative and associative and that `S(n) = n + 1`.  The pragmatic
reasons for the definitions of `(+)` and `(*)` become secondary to the natural
generality of the abstractions based on them.

The term "natural" can be loaded, especially in regards to a computer language
which draws heavily on notions from category theory.  So, what "natural" is
taken to mean will be partially based on the choices made in Haskell's lazy
evaluation strategy.

Consider the true mathematical statements, which follow from the distributive
theorem of Natural Numbers, and are equal via the commutative theorem, 
```
   x*(y + 1) = x*y + x                x*(1 + y) = x + x*y
```
from these we abstract competing definitions for `(*)`
```
   x * S y = x * y + x                x * S y = x + x * y
```
My intuition is leaning toward the first one due to Haskell's left-to-right
evaluation strategy.  The recursion generates the repeated addition before
adding up the terms generated.  I find the separation of concerns more
appealing.  At this point, not clear to me which approach is "lazier."

### Initial attempt
Consider the evaluation of `three * three`,
let `z = Zero

#### Version originally suggested:
```
x + Zero = x
x + S y = S (x + y)

_ * Zero = Zero
x * S y  = x + x * y
```
```
three * three
S(S(S(z))) * S(S(S(z)))
S(S(S(z))) + S(S(S(z))) * S(S(z))
S(S(S(z))) + S(S(S(z))) + S(S(S(z))) * S(z)
S(S(S(S(z))) + S(S(z))) + S(S(S(z))) * S(z)
S(S(S(S(S(z))) + S(z))) + S(S(S(z))) * S(z)
S(S(S(S(S(S(z))) + z))) + S(S(S(z))) * S(z)
S(S(S(S(S(S(z)))))) + S(S(S(z))) * S(z)
S(S(S(S(S(S(z)))))) + S(S(S(z))) + S(S(S(z))) * z
S(S(S(S(S(S(z)))))) + S(S(S(z))) + z
S(S(S(S(S(S(S(z)))))) + S(S(z))) + z
S(S(S(S(S(S(S(z)))))) + S(S(z)))           -- evaluated to type constructor

S(S(S(S(S(S(S(S(z)))))) + S(z)))
S(S(S(S(S(S(S(S(S(z)))))) + z)))
S(S(S(S(S(S(S(S(S(z)))))))))               -- fully evaluated
nine
```

#### Improved version suggested:
```
x + Zero = x
x + S y = S (x + y)

_ * Zero = Zero
x * S y  = x * y + x
```
```
three * three
S(S(S(z))) * S(S(S(z)))
S(S(S(z))) * S(S(z)) + S(S(S(z))) 
S(S(S(z))) * S(z) + S(S(S(z))) + S(S(S(z))) 
S(S(S(z))) * z + S(S(S(z))) + S(S(S(z))) + S(S(S(z))) 
z + S(S(S(z))) + S(S(S(z))) + S(S(S(z))) 
S(z + S(S(z))) + S(S(S(z))) + S(S(S(z)))
S(S(z + S(S(z)) + S(S(z)))) + S(S(S(z)))
S(S(S(z + S(S(z)) + S(S(z)))) + S(S(z)))   -- evaluated to type constructor

S(S(S(S(z + S(S(z)) + S(S(z)))) + S(z)))
S(S(S(S(S(z + S(S(z)) + S(S(z)))) + z)))
S(S(S(S(S(z + S(S(z)) + S(S(z)))))))
S(S(S(S(S(S(z + S(z)) + S(S(z)))))))
S(S(S(S(S(S(S(z + S(z)) + S(z)))))))
S(S(S(S(S(S(S(S(z + S(z)) + z)))))))
S(S(S(S(S(S(S(S(z + S(z)))))))))
S(S(S(S(S(S(S(S(S(z + z)))))))))
S(S(S(S(S(S(S(S(S(z)))))))))               -- fully evaluated
nine
```

#### Redundant version
```
x + Zero = x
Zero + x = x
x + S y = S (x + y)

_ * Zero = Zero
Zero * _ = Zero
x * S y  = x * y + x
```
three * three
S(S(S(z))) * S(S(S(z)))
S(S(S(z))) * S(S(z)) + S(S(S(z)))
S(S(S(z))) * S(z) + S(S(S(z))) + S(S(S(z)))
S(S(S(z))) * z + S(S(S(z))) + S(S(S(z))) + S(S(S(z)))
z + S(S(S(z))) + S(S(S(z))) + S(S(S(z)))
S(S(S(z))) + S(S(S(z))) + S(S(S(z)))
S(S(S(S(z))) + S(S(z))) + S(S(S(z)))
S(S(S(S(S(z))) + S(S(z))) + S(S(z)))       -- evaluated to type constructor

S(S(S(S(S(S(z))) + S(S(z))) + S(z)))
S(S(S(S(S(S(S(z))) + S(S(z))) + z)))
S(S(S(S(S(S(S(z))) + S(S(z))))))
S(S(S(S(S(S(S(S(z))) + S(z))))))
S(S(S(S(S(S(S(S(S(z))) + z))))))
S(S(S(S(S(S(S(S(S(z)))))))))               -- fully evaluated
nine
```
Interesting, initially not "lazier" but more efficient if driven to full evaluation.

### Follow up to above based on lecture example.
Consider the evaluation of `three * three == one`,
let `z = Zero

#### Version originally suggested:
```
x + Zero = x
x + S y = S (x + y)

_ * Zero = Zero
x * S y  = x + x * y
```
```
three * three == two
S(S(S(z))) * S(S(S(z))) == S(S(z))
S(S(S(z))) + S(S(S(z))) * S(S(z)) == S(S(z))
S(S(S(z))) + S(S(S(z))) + S(S(S(z))) * S(z) == S(S(z))
S(S(S(S(z))) + S(S(z))) + S(S(S(z))) * S(z) == S(S(z))
S(S(S(S(S(z))) + S(z))) + S(S(S(z))) * S(z) == S(S(z))
S(S(S(S(S(S(z))) + z))) + S(S(S(z))) * S(z) == S(S(z))
S(S(S(S(S(S(z)))))) + S(S(S(z))) * S(z) == S(S(z))
S(S(S(S(S(S(z)))))) + S(S(S(z))) + S(S(S(z))) * z == S(S(z))
S(S(S(S(S(S(z)))))) + S(S(S(z))) + z == S(S(z))
S(S(S(S(S(S(S(z)))))) + S(S(z))) + z == S(S(z))
S(S(S(S(S(S(S(z)))))) + S(S(z))) == S(S(z))
S(S(S(S(S(S(z)))))) + S(S(z)) == S(z)
S(S(S(S(S(S(S(S(z)))))) + S(z))) == S(z)
S(S(S(S(S(S(S(z)))))) + S(z)) == Zero
False
```

#### Improved version suggested:
```
x + Zero = x
x + S y = S (x + y)

_ * Zero = Zero
x * S y  = x * y + x
```
```
three * three == two
S(S(S(z))) * S(S(S(z))) == S(S(z))
S(S(S(z))) * S(S(z)) + S(S(S(z))) == S(S(z))
S(S(S(z))) * S(z) + S(S(S(z))) + S(S(S(z))) == S(S(z))
S(S(S(z))) * z + S(S(S(z))) + S(S(S(z))) + S(S(S(z))) == S(S(z))
z + S(S(S(z))) + S(S(S(z))) + S(S(S(z))) == S(S(z))
S(z + S(S(z))) + S(S(S(z))) + S(S(S(z))) == S(S(z))
S(S(z + S(S(z)) + S(S(z)))) + S(S(S(z))) == S(S(z))
S(S(S(z + S(S(z)) + S(S(z)))) + S(S(z))) == S(S(z))
S(S(z + S(S(z)) + S(S(z)))) + S(S(z)) == S(z)
S(S(z + S(S(z)) + S(S(z)))) + S(S(z)) == S(z)
S(S(S(z + S(S(z)) + S(S(z)))) + S(z)) == S(z)
S(S(z + S(S(z)) + S(S(z)))) + S(z) == z
S(S(S(z + S(S(z)) + S(S(z)))) + z) == Zero
False
```

#### Redundant version
```
x + Zero = x
Zero + x = x
x + S y = S (x + y)

_ * Zero = Zero
Zero * _ = Zero
x * S y  = x * y + x
```
three * three == two
S(S(S(z))) * S(S(S(z))) == S(S(z))
S(S(S(z))) * S(S(z)) + S(S(S(z))) == S(S(z))
S(S(S(z))) * S(z) + S(S(S(z))) + S(S(S(z))) == S(S(z))
S(S(S(z))) * z + S(S(S(z))) + S(S(S(z))) + S(S(S(z))) == S(S(z))
z + S(S(S(z))) + S(S(S(z))) + S(S(S(z))) == S(S(z))
S(S(S(z))) + S(S(S(z))) + S(S(S(z))) == S(S(z))
S(S(S(S(z))) + S(S(z))) + S(S(S(z))) == S(S(z))
S(S(S(S(S(z))) + S(S(z))) + S(S(z))) == S(S(z))
S(S(S(S(z))) + S(S(z))) + S(S(z)) == S(z)
S(S(S(S(S(z))) + S(S(z))) + S(z)) == S(z)
S(S(S(S(z))) + S(S(z))) + S(z) == z
S(S(S(S(S(z))) + S(S(z))) + z) == Zero
False
```
