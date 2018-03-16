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

    (<=) Zero _ = True
    (<=) _ Zero = False
    (<=) (Succ m) (Succ n) = m <= n

    -- Ord does not require defining min and max.
    -- Howevener, you should define them without using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min m n = if m <= n then m else n

    max m n = if m <= n then n else m

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
(<*>) Zero _     = Zero
(<*>) _ Zero     = Zero
(<*>) n (Succ m) = n <+> (n <*> m)

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
(<^>) _ Zero     = Succ Zero
(<^>) n (Succ m) = n <*> (n <^> m)

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) Zero _ = Zero
(</>) _ Zero = error "Division by zero"
(</>) n m
  | n == m    = Succ Zero
  | n <= m    = Zero
  | otherwise = Succ $ n <-> m </> m

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) Zero _ = Zero
(<%>) _ Zero = error "Division by zero"
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
factorial Zero     = Succ Zero
factorial (Succ n) = Succ n <*> (factorial n)

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg Zero = Zero
sg _    = Succ Zero

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo _ Zero = undefined
lo Zero _ = Zero
lo (Succ Zero) _ = undefined
lo _ (Succ Zero) = Zero
lo n m = if m >= n then Succ $ lo n (m </> n) else Zero

--
-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!
--

toNat :: Integral a => a -> Nat
toNat 0 = Zero
toNat n
  | n < 0 = error "Cannot assign a negative value to a Natural"
  | otherwise = Succ $ toNat (n - 1)

fromNat :: Integral a => Nat -> a
fromNat Zero = 0
fromNat (Succ n) = 1 + fromNat n


-- Obs: we can now easily make Nat an instance of Num.
instance Num Nat where

  (+) = (<+>)
  (*) = (<*>)
  (-) = (<->)
  abs n = n
  signum = sg
  fromInteger x
    | x < 0     = error "Cannot assign a negative value to a Natural"
    | x == 0    = Zero
    | otherwise = Succ $ fromInteger (x - 1)
