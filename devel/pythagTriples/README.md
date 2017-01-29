##A Pythagorean Triple generation program implemented in Haskell using the stack buildtool.

A Pythagoean Triple is a tuple `(a,b,c)` such that `a^2 + b^2 = c^2`.

This program generates all possible pythagorean triples such that `gcd(a,b,c) = 1` and `a, b, c > 0`.

###Usage:
  ```
  pythagTriples [-o|-f|-fs|-h] number
    where
      number = number of triples to print
    and
      -o  Triples (a, b, c) are printed in lexiconical order,
          that is a < b < c, and a,b,c have no common factors.
          Algorithm does prints all possible b's and c's before
          going onto next a.
      -f  Use fast algorithm where triples (a, b, c) are such
          that a is odd, b is even, and a,b,c have no common
          factors.
      -fs Use fast algorithm, but sort results a < b < c.
      -h  Print help message.
  ```   
  Both algorithms only print triples with no common factors, that is `gcd(a,b,c) = 1`.

###Some design considerations:

  1. These algorithms generate pathagorean triples with no common
     factors.
      ```
       a^2 + b^2 = c^2  where gcd(a, b, c) = 1 and 0 < a < b < c
      ```
     you only need to check any two of a,b,c because you can factor
     the common factor to show the the square of the other (and
     hence the other itself) has the same common factor.

     Geometrically this is the right choice since as right triangles
       ```
       (3,4,5) and (6,8,10) and (4,3,5) 
       ```
     are similar and hence the same except for scale or orientation.

  2. There is no such thing as a equilateral pyathogorean triangle.
       ```
       a^2 + a^2 = c^2 => 2*a^2 = c^2 => sqrt(2) = c/a
       ```
     but the square root of 2 is not rational!

  3. The ordered algorthm generates all possible pythagorithms in
     lexiconical order, that is `a < b < c`.

     As b gets larger, eventually the difference in length beween
     c and b is less than 1.

     ```
               *
               *    *       c            As b gets bigger, eventially
       fixed a *         *               c - b < 1 => no more triples.
               *               *
               *                    *
               * * * * * * * * * * * * * 
                      vary b
     ```
     So,

       ```
       a^2 + b^2 = c^2
       a^2 = c^2 - b^2 = (c-b)*(c+b) < c + b
       a^2 < sqrt(a^2 + b^2) + b
       (a^2 - b)^2 < a^2 + b^2
       a^4 - 2*a^2*b + b^2 < a^2 + b^2
       a^2 - 2*b < 1
       b > (a^2 - 1)/2
       ```
     Therefore, we only need to check values of b for
       ```
       a+1 <= b <= (a^2 - 1)/2
       ```
     From running code, we see that both <= cases happen.

  4. Running code it seems that the hypotence c is alwaya odd.
     To see that this is universally true:

     We know not both a and b even, otherwise a,b,c not in lowest
     terms.  If one even and the other odd, then c is odd.

     But what about the case if a and b both odd?  That would
     imply c could be even.  Concider this case,
       ```
       a^2 + b^2 = c^2
       (2*m+1)^2 + (2*n+1)^2 = (2*p)^2
       4*m^2 + 4*m + 1 + 4*n^2 + 4*n + 1 = 4*p^2
       4*(m^2 + n^2) + 4*(m + n) + 2 = 4*p^2
       2*(m^2 + n^2) + 2*(m + n) + 1 = 2*p^2
       1 = 2*( p^2 - m^2 - n^2 - m - n)
       ```
     But 1 is not even, therefore c always odd.

     Thus, if a is odd, we need to only check for b even, and via versa.

  5. I came across a faster algorithm which produces unordered results,
       ```
       [(m^2 - n^2, 2*m*n, m^2 + n^2) | m <- [1 ..] , n <- [1 .. m-1]] 
       ```
     Also seems to produce results where `gcd(a,b,c) > 1`
     like `(8,6,10)` and `(40,42,58)`.

     I used this result for a faster algorithm and will use
     test/Spec.hs to compare the results.  So far the fast
     algorithm seems complete, but it not clear yet how far
     you have to take it out to ensure you catch all triples
     for a given a.  Also, if you sort the results of the fast
     algorithm, is there a point that the cost of sorting is
     so high that the ordered algorithm wins out?
