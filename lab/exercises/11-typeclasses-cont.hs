{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2

import Prelude hiding (Semigroup(..), Monoid(..), foldMap, all, any)

-- REMINDER:

--class Eq a where -- == or /=
--  (==) :: a -> a -> Bool
--  x == y = not (x /= y)
--  (/=) :: a -> a -> Bool
--  x /= y = not (x == y)
--
-- Eq laws
-- for all x y z:
-- reflexivity - x == x
-- symmetry -  x == y -> y == x
-- transitivity - x == y && y == z -> x == z

--class Eq a => Ord a where -- <= or compare
--  compare :: a -> a -> Ordering
--  compare x y
--    | x == y = EQ
--    | x <= y = LT
--    | otherwise = GT
--  (<=) :: a -> a -> Bool
--  x <= y = case compare x y of
--            LT -> True
--            EQ -> True
--            GT -> False
--
--  Ord laws - should probably be a partial order
--  for all x y z:
--  reflexivity - x <= x
--  antisymmetry - x <= y & y <= x -> x == y
--  transitivity - x <= y & y <= z -> x <= z

class Monoid a where
  zero :: a
  (<>) :: a -> a -> a
-- Monoid laws:
-- for all x y z:
-- zero is identity - zero <> x == x == x <> zero
-- (<>) is associative - (x <> y) <> z == x <> (y <> z)

infixr 6 <>
--
-- TODO: mention typeclassopedia
-- https://wiki.haskell.org/Typeclassopedia


--N, 0, +
newtype Add = Add Int

getAdd :: Add -> Int
getAdd (Add x) = x

instance Monoid Add where
  zero :: Add
  zero = Add 0
  (<>) :: Add -> Add -> Add
  (<>) (Add x) (Add y) = Add (x + y)

--N, 1, *
newtype Mult = Mult Int

getMult :: Mult -> Int
getMult (Mult x) = x

instance Monoid Mult where
  zero :: Mult
  zero = Mult 1
  (<>) :: Mult -> Mult -> Mult
  (<>) (Mult x) (Mult y) = Mult (x * y)


-- TODO: newtypes (single instance, examples: Add, Mult)
-- TODO: mention InstanceSigs
-- with example

-- Use Naught instead of Zero so we can use Zero for bits
data Nat = Naught | Succ Nat
  deriving Show

-- partial, but convenient instance of Num for Nat
-- so we can write number literals to mean Nat numbers
-- @fromInteger@ is the convenient bit
instance Num Nat where
  fromInteger :: Integer -> Nat
  fromInteger 0 = Naught
  fromInteger n = Succ $ fromInteger $ n - 1
  (+) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  negate = undefined

-- EXERCISE: Equality for Nats
instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  (==) Naught Naught = True
  (==) Naught _ = False
  (==) _ Naught = False
  (==) (Succ n) (Succ m) = (==) n m


-- EXERCISE: Ordering for Nats
instance Ord Nat where
  -- choose one to implement, you can delete the other one
  (<=) :: Nat -> Nat -> Bool
  (<=) Naught _ = True
  (<=) _ Naught = False
  (<=) (Succ a) (Succ b) = (<=) a b

-- EXERCISE: Addition on Nats
-- EXAMPLES:
-- 0 <> 13 = 13
-- 5 <> 10 == 15
instance Monoid Nat where
  zero :: Nat
  zero = Naught
  (<>) :: Nat -> Nat -> Nat
  (<>) Naught b = b
  (<>) (Succ a) b = (<>) a (Succ b)

-- EXERCISE: Appending lists
-- EXAMPLES:
-- [1,2,3] <> [] == [1,2,3]
-- [1,2,3] <> [4,5,6] == [1,2,3,4,5,6]
instance Monoid [a] where
  zero :: [a]
  zero = []
  (<>) :: [a] -> [a] -> [a]
  (<>) [] b = b
  (<>) (x:xs) b = x : ((<>) xs b)

------ MONOID USAGE

-- EXERCISE: Adding a value multiple times
--
-- EXAMPLES:
-- repeatMonoid (Succ (Succ (Succ Naught))) (Succ (Succ Naught)) == (Succ (Succ (Succ (Succ (Succ (Succ Naught)))))) (6 as a Nat)
-- repeatMonoid 3 [1,2,3] == [1,2,3,1,2,3,1,2,3]
repeatMonoid :: Monoid a => Nat -> a -> a
repeatMonoid Naught _ = zero
repeatMonoid (Succ y) x = (<>) x (repeatMonoid y x)

-- EXERCISE: Concatenate a list using a monoid
--
-- EXAMPLES:
-- monoidConcat [[1,2,3],[4,5,6],[7,8,9]] == [1,2,3,4,5,6,7,8,9]
-- monoidConcat [(1 :: Nat),2,3] == <6 as a nat>
monoidConcat :: Monoid a => [a] -> a
--monoidConcat [] = zero
--monoidConcat [x] = x
--monoidConcat (x:xs) = (<>) x (monoidConcat xs)
monoidConcat = foldr (<>) zero

-- EXERCISE: One Fun to rule them all, One Fun to find them, One Fun to bring them all and in the darkness bind them
--
-- EXAMPLES:
-- foldMap integerToNat [1,2,3] == <6 as a Nat>
-- foldMap (`repeatMonoid` [1,2]) [1,2,3] == [1,2 1,2,1,2, 1,2,1,2,1,2]
foldMap :: (Monoid b) => (a -> b) -> [a] -> b
foldMap f = monoidConcat . map f
--foldMap = owl monoidConcat map
--foldMap = (monoidConcat .) . mapz
--owl = (.) . (.)
--owl = fmap fmap fmap
------ ENDO

-- Endo means "from one thing to itself"
-- We use this datatype as a wrapper,
-- so we can easily create a Monoid instance for this type.
-- Think about the types!
newtype Endo a = Endo (a -> a)

getEndo :: Endo a -> a -> a
getEndo (Endo f) = f

-- EXERCISE: Endofunctions are a monoid
--
-- getEndo (Endo (+1) <> Endo (+5)) 10 == 16
-- getEndo (foldMap Endo [(+1), (*10), (`div` 2)]) 32 == 165
instance Monoid (Endo a) where
  zero :: Endo a
  zero = Endo (\x -> x)
  (<>) :: Endo a -> Endo a -> Endo a
  (<>) e1 e2 = Endo (getEndo e2 . (getEndo e1))

------ BITVECTOR INSTANCES

data Bit = Zero | One
  deriving (Eq, Ord, Show)

-- your favourite
data BitVector
  = End
  | BitVector :. Bit
  deriving Show

infixl 6 :.

-- This will be useful
canonicalise :: BitVector -> BitVector
canonicalise End = End
canonicalise (bv :. Zero) = case canonicalise bv of
  End -> End
  bv' -> bv' :. Zero
canonicalise (bv :. One) = canonicalise bv :. One


-- N.B.!!!:
-- We will want to canonicalise all BitVectors in both Eq and Ord before doing comparisons with them
-- for our laws to hold for both our instances.

-- EXERCISE: Equality on BitVectors
--
-- EXAMPLES:
-- End == End :. Zero == True
-- End :. One :. Zero == End :. One == False
instance Eq BitVector where
  (==) :: BitVector -> BitVector -> Bool
  (==) = undefined

-- Same as above - a convenient way to write number literals instead of BitVectors
-- Hacky but convenient!
-- define only @fromInteger@
instance Num BitVector where
  fromInteger :: Integer -> BitVector
  fromInteger 0 = End
  fromInteger n = fromInteger (n `div` 2) :. if n `rem` 2 == 0 then Zero else One
  (+) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  negate = undefined

-- EXERCISE: Adding Bitvectors
-- use bitvector addition for this!
--
-- EXAMPLES:
-- End <> End :. One -- End :. One
-- End :. Zero <> End -- End :. Zero
-- End :. Zero <> End :. One -- End :. One
-- End :. One <> End :. Zero -- End :. One
-- End :. One <> End :. One :. One -- End :. One :. Zero :. Zero
instance Monoid BitVector where
  zero :: BitVector
  zero = undefined
  (<>) :: BitVector -> BitVector -> BitVector
  (<>) = undefined

-- Order BitVectors as the numbers they represent
-- (in other words
-- bv1 < bv2 <=> (bitVectorToInteger bv1) < (bitVectorToInteger bv2)
-- but don't use bitVectorToInteger :P)

-- This is a bit tricky
-- Canonicalise them first!

-- You can run a test by uncommenting the quickcheck import up top
-- and writing `quickCheck prop_compareWorksBitVector`
-- in ghci
-- The test checks precisely the condition above, for randomly generated
-- canonical bitvectors.
instance Ord BitVector where
  -- choose one to implement, you can delete the other one
  compare :: BitVector -> BitVector -> Ordering
  compare = undefined
  (<=) :: BitVector -> BitVector -> Bool
  (<=) = undefined

-- Uncomment below here if you want tests
--instance Arbitrary Bit where
--  arbitrary = elements [Zero, One]
--
--instance Arbitrary BitVector where
--  arbitrary = (canonicalise . listToBv) <$> listOf arbitrary
--    where
--      listToBv :: [Bit] -> BitVector
--      listToBv [] = End
--      listToBv (b:bs) = listToBv bs :. b
--
--prop_compareWorksBitVector :: BitVector -> BitVector -> Bool
--prop_compareWorksBitVector bv1 bv2 =
--  compare bv1 bv2 == (compare `on` bitVectorToInteger) bv1 bv2
--  where
--    bitVectorToInteger :: BitVector -> Integer
--    bitVectorToInteger = go . canonicalise
--      where
--        go :: BitVector -> Integer
--        go End = 0
--        go (bv :. Zero) = 2 * go bv
--        go (bv :. One) = 1 + 2 * go bv


-- EXERCISE: Booleans, but a monoid under (&&)
--
-- EXAMPLES:
-- All False <> All True == All False
-- monoidConcat [All True, All True, All True] == All True
-- monoidConcat [All True, All True, All False] == All False
newtype All = All Bool
  deriving Show

getAll :: All -> Bool
getAll (All x) = x

instance Monoid All where
  zero :: All
  zero = undefined
  (<>) :: All -> All -> All
  (<>) = undefined

-- EXERCISE: all, using All
-- Use foldMap and All to define all
all :: (a -> Bool) -> [a] -> Bool
all = undefined

-- EXERCISE: Booleans, but a monoid under (||)
--
-- EXAMPLES:
-- Any False <> Any True == Any True
-- monoidConcat [Any True, Any False, Any False] == Any True
-- monoidConcat [Any False, Any False, Any False] == Any False
newtype Any = Any Bool
  deriving Show

getAny :: Any -> Bool
getAny (Any x) = x

instance Monoid Any where
  zero :: Any
  zero = undefined
  (<>) :: Any -> Any -> Any
  (<>) = undefined

-- EXERCISE: any, using Any
-- Use foldMap and Any to define any
any :: (a -> Bool) -> [a] -> Bool
any = undefined

-- EXERCISE: Merge monoid
-- This is a "party trick" monoid, which we can use to implement merge sort.
-- Our idea is that we will hold lists, but instead of the usual
-- concatenation monoid that we have for lists, we will use the
-- "merge" part of merge sort to implement our Monoid instance.
newtype Merge a = Merge [a]
  deriving Show

getMerge :: Merge a -> [a]
getMerge (Merge xs) = xs

-- Assume that the lists inside Merge are sorted!
instance Ord a => Monoid (Merge a) where
  zero :: Merge a
  zero = undefined
  (<>) :: Merge a -> Merge a -> Merge a
  (<>) = undefined
    where
      merge :: [a] -> [a] -> [a]
      merge = undefined

-- EXERCISE: Merge sort, using Merge
-- Implement merge sort by using the Merge monoid and foldMap
mergeSort :: [a] -> [a]
mergeSort = undefined
