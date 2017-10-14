-- NaturalNumbers.hs

-- | A module implementating the Natural Numbers within the Haskell
--   type system using the Peano-Dedekind Axioms for arithmetic.

module NaturalNumber where

-- | Data structure representing the natural number 0,
--   or a successor of a natural number.

data NaturalNumber = Zero | S NaturalNumber
  deriving (Show)

-- Common names for some small natural numbers.

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

-- Some examples of a non-grounded data type.

infinity = S infinity 

loop :: NaturalNumber
loop = loop

-- | NaturalNumber is an instance of typeclass EQ.

instance Eq NaturalNumber where
    S x  == S y  = x == y
    Zero == Zero = True
    Zero == S _  = False
    S _  == Zero = False

-- | NaturalNumber is an instance of typeclass Ord

instance Ord NaturalNumber where
    compare (S a) (S b) = compare a b
    compare Zero Zero   = EQ
    compare Zero _      = LT 
    compare _    Zero   = GT 

-- | NaturalNumber is an instance of typeclass Num

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

-- | A NaturalNumber context helper function.
--
--   In the expression "nat (4 + 3*5)" 3, 4, and 5 are interpretted as
--   NaturalNumbers.  Also, "nat (-5)" is "Zero", consistent with our
--   notion of subtraction.  The magic is in the type signature and how
--   negation is defined in the Num typeclass.

nat :: NaturalNumber -> NaturalNumber
nat = id

-- | Determine if a natural number is even.
--
--   Unsafe for Fractional data types if typed
--     even' :: (Eq t, Num t) => t -> Bool
--   and the name "even" collides with the one from the
--   Integral typeclass imported from Prelude. 

even' :: NaturalNumber -> Bool
even' n
    | n == 0 = True
    | n == 1 = False
    | otherwise = even' $ n-2

-- | Determine if a natural number is odd.

odd' :: NaturalNumber -> Bool
odd' = not . even'
