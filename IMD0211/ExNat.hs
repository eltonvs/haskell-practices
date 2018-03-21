module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral
    , Bool(..)
    , not
    , (&&)
    , (||)
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat = Zero | Succ Nat

instance Show Nat where

    show Zero     = "0"
    show (Succ n) = "S" ++ show n

instance Eq Nat where

    Zero   == Zero    = True
    Zero   == Succ _  = False
    Succ _ == Zero    = False
    Succ m == Succ n  = m == n

instance Ord Nat where

    (<=) Zero _            = True
    (<=) _ Zero            = False
    (<=) (Succ m) (Succ n) = m <= n

    -- Ord does not require defining min and max.
    -- Howevener, you should define them without using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min _ Zero            = Zero
    min Zero _            = Zero
    min (Succ n) (Succ m) = Succ $ min n m

    max n Zero            = n
    max Zero n            = n
    max (Succ n) (Succ m) = Succ $ max n m

isZero :: Nat -> Bool
isZero Zero = True
isZero _    = False

pred :: Nat -> Nat
pred Zero     = Zero
pred (Succ x) = x

even :: Nat -> Bool
even Zero            = True
even (Succ Zero)     = False
even (Succ (Succ x)) = even x

odd :: Nat -> Bool
odd = not . even

-- addition
(<+>) :: Nat -> Nat -> Nat
(<+>) Zero n     = n
(<+>) n Zero     = n
(<+>) n (Succ m) = Succ n <+> m

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when subtraction returns a negative number.
(<->) :: Nat -> Nat -> Nat
(<->) Zero _            = Zero
(<->) n Zero            = n
(<->) (Succ n) (Succ m) = n <-> m

-- multiplication
(<*>) :: Nat -> Nat -> Nat
(<*>) n m = mult' n m Zero where
  mult' Zero _ acc     = acc
  mult' _ Zero acc     = acc
  mult' n (Succ m) acc = mult' n m (n <+> acc)

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
(<^>) n m = pow' n m (Succ Zero) where
  pow' _ Zero acc     = acc
  pow' n (Succ m) acc = pow' n m (n <*> acc)

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) n m = quot' n m Zero where
  quot' n (Succ Zero) _ = n
  quot' _ Zero _        = error "Division by zero"
  quot' Zero _ acc      = acc
  quot' n m acc
    | n == m    = Succ acc
    | n < m     = acc
    | otherwise = quot' (n <-> m) m (Succ acc)

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) _ Zero = error "Division by zero"
(<%>) Zero _ = Zero
(<%>) n m
  | n == m    = Zero
  | n <= m    = n
  | otherwise = n <-> m <%> m

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) n m = n <%> m == Zero

divides = (<|>)


-- x `absDiff` y = |x - y|
-- (Careful: here this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff Zero n            = n
absDiff n Zero            = n
absDiff (Succ n) (Succ m) = absDiff n m

(|-|) = absDiff

factorial :: Nat -> Nat
factorial n = factorial' n (Succ Zero) where
  factorial' Zero acc     = acc
  factorial' (Succ n) acc = factorial' n (acc <*> (Succ n))

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg Zero = Zero
sg _    = Succ Zero

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo n m = lo' n m Zero where
  lo' _ Zero _          = undefined
  lo' Zero _ acc        = acc
  lo' (Succ Zero) _ _   = undefined
  lo' _ (Succ Zero) acc = acc
  lo' n m acc           = if m >= n then lo' n (m </> n) (Succ acc) else acc

--
-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!
--

toNat :: Integral a => a -> Nat
toNat 0 = Zero
toNat n
  | n < 0     = error "Cannot assign a negative value to a Natural"
  | otherwise = Succ $ toNat (n - 1)

fromNat :: Integral a => Nat -> a
fromNat Zero     = 0
fromNat (Succ n) = 1 + fromNat n


-- Obs: we can now easily make Nat an instance of Num.
instance Num Nat where

  (+) = (<+>)
  (*) = (<*>)
  (-) = (<->)
  abs n = n
  signum = sg
  fromInteger = toNat
