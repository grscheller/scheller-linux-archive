{- NaturalNumber.hs

   Haskell implementation of the Natural Numbers within the Haskell
   type system using the Peano-Dedekind Axioms for arithmetic.
 -} 

module NaturalNumber where

data NaturalNumber = Zero | S NaturalNumber
  deriving (Show)

-- common names for some small natural numbers
--   These are "variables" bounded to values denoted by expressions.

zero   = Zero
one    = S zero
two    = S one
three  = S two
four   = S three
five   = S four
six    = S five
seven  = S six
eight  = S seven
nine   = S eight
ten    = S nine
eleven = S ten
twelve = S eleven

-- Some examples of a non-grounded data type

infinity = S infinity 

loop :: NaturalNumber
loop = loop

-- Make NaturalNumber an instance of EQ
instance Eq NaturalNumber where
  S x  == S y  = x == y
  Zero == Zero = True
  Zero == S _  = False
  S _  == Zero = False

-- Make NaturalNumber an instance of Ord
instance Ord NaturalNumber where
  compare (S a) (S b) = compare a b
  compare Zero Zero   = EQ
  compare Zero _      = LT 
  compare _    Zero   = GT 

-- Make NaturalNumber an instance of Num
instance Num NaturalNumber where
  x + Zero = x
  x + S y  = S (x + y)

  Zero * _    = Zero
  _    * Zero = Zero
  x    * S y  = x * y + x

  Zero  - _     = Zero
  x     - Zero  = x
  (S x) - (S y) = x - y

  abs = id

  signum Zero  = Zero
  signum (S _) = S(Zero)

  fromInteger n
      | n > 0  = S (fromInteger (n-1))
      | n == 0 = Zero

-- NaturalNumber helper function
--   In nat (4 + 3*5) forces 3, 4, 5 to be interpretted as NaturalNumbers.
--   Also, nat (-5) = Zero, consistent with our notion of subtraction.
--   The magic is in the type signature and how negation is defined in
--   the Num typeclass.
nat :: NaturalNumber -> NaturalNumber
nat = id

-- Unsafe for Fractional data types if typed
--   even' :: (Eq t, Num t) => t -> Bool
-- and the name "even" collides with the one from the
-- Integral typeclass imported from Prelude. 
even' :: NaturalNumber -> Bool
even' n
    | n == 0 = True
    | n == 1 = False
    | otherwise = even' $ n-2

odd' :: NaturalNumber -> Bool
odd' = not . even'
