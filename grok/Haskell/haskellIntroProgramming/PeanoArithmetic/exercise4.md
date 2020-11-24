# Compare implementations for multiplication (Exercise 4)

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

## Mathematical background and the "Natural" definition of `(*)`

The above are minimalist definitions set up to prove theorems about natural
numbers, typically using mathematical induction.  One proves theorems such
as `S(n) = n + 1` or that `(*)` is commutative and associative.  The pragmatic
reasons for the definitions of `(+)` and `(*)` become secondary to the natural
generality of the abstractions based on them.

The term "natural" can be loaded, especially in regards to a computer language
which draws heavily on notions from category theory.  So, what "natural" is
taken to mean will be partially based on the choices made in Haskell's lazy
evaluation strategy.

Consider the true mathematical statements, which follow from the distributive
theorem of Natural Numbers, and are equalivalent via the commutative theorem
for `+`,

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
appealing.  Also, I don't see how a partial evaluation of `(*)` would be
that useful.  When do we operate on partial multiplications?  Maybe there are
advantages steamlining the calculation or making things less stack cranky
in the second approach?

At this point, not clear to me which approach is "lazier."

## Initial attempt

Consider the evaluation of `three * three`, where we are letting `z = Zero`

### Originally version suggested

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

Almost completely evaluated until I get down to a type constructor.

### Improved version suggested

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

Three substitutions lazier, but total number of steps to full evaluation longer.

### Redundant version of improved version

```
x + Zero = x
Zero + x = x
x + S y = S (x + y)

_ * Zero = Zero
Zero * _ = Zero
x * S y  = x * y + x
```

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

Interesting, not any "lazier" than the non-redundant version, but more
efficient if driven to full evaluation.

## Follow up to above based on lecture example

Consider the evaluation of `three * three == two`,

### Version originally suggested

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

Done in 14 evaluations.

### Suggested improved version

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

Done in 13 evaluations.

### Redundant version

```
x + Zero = x
Zero + x = x
x + S y = S (x + y)

_ * Zero = Zero
Zero * _ = Zero
x * S y  = x * y + x
```

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

Done in 12 evaluations.

