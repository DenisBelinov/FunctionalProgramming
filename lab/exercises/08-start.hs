{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2

-- TODO: ask about additional exercises, because we're skipping two due to new years stuff
-- TODO: extend deadline?

import Prelude hiding (concat, map, filter, foldl, foldr, takeWhile, dropWhile)

----------------------------- Misc. list stuff -------------------------------------

-- EXERCISE: Map
-- EXAMPLES:
-- map (+5) [] -- []
-- map (+5) [1,2,3] -- [6,7,8]
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

-- EXERCISE: Filter
-- EXAMPLES:
-- filter even [1,2,3] -- [2]
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs) = if (f x)
                  then x : filter f xs
                  else filter f xs

-- EXERCISE: Fold recursion
-- EXAMPLES:
-- foldr (+) 0 [1..100] -- 5050
-- foldr (-) 15 [1..5] -- -12
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ nv [] = nv
foldr f nv (x:xs) = f x (foldr f nv xs)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v [] = v
foldr' f v (x:xs) = foldr' f (f x v) xs

-- EXERCISE: Fold "iterative"
-- foldl (+) 0 [1..100] -- 5050
-- foldl (-) 15 [1..5] -- 0
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ nv [] = nv
foldl f nv (x:xs) = foldl f (f nv x) xs

-- EXERCISE: Generating a range
-- The [a..b] syntax is syntactic sugar for this
-- (so [a..b] is equivalent to fromTo a b)
-- EXAMPLES:
-- fromTo 1 5 -- [1, 2, 3, 4, 5]
-- fromTo 6 5 -- []
fromTo :: Integer -> Integer -> [Integer]
fromTo a b = if a > b
             then []
             else a : (fromTo (a + 1) b)

-- EXERCISE: Generating a range with a diff
-- The [a, a1, ..b] syntax is syntactic sugar for this
-- (so [a, a1..b] is equivalent to deltaFromTo (a1 - a) a b)
-- EXAMPLES:
-- delteFromTo 2 1 5 -- [1, 3, 5]
-- delteFromTo 24 1 101 -- [1,25,49,73,97]
fromThenTo :: Integer -> Integer -> Integer -> [Integer]
fromThenTo diff a b = if a > b
                      then []
                      else a : (fromThenTo diff (a + diff) b)

-- EXERCISE: Interleaving lists
-- Take turns in taking elements from lists.
-- If one of them runs out just take the rest of the one that hasn't run out.
-- EXAMPLES:
-- interleave [1,3..10] [2,4..10] -- [1,2,3,4,5,6,7,8,9,10]
-- interleave [1,3..10] [2,4..8] -- [1,2,3,4,5,6,7,8,9]
-- interleave [1,3..7] [2,4..10] -- [1,2,3,4,5,6,7,8,10]
interleave :: [a] -> [a] -> [a]
interleave [] l = l
interleave (x:xs) l = x : interleave l xs
-- HINT: You don't need to do matching on both of them

-- EXERCISE: Something weird
-- Our goal is to include all the members of every list *eventually*.
-- This is useful when working with infinite lists (later)
-- EXAMPLES:
-- megaInterleave [[1,4..20], [2,5..20], [3,6..20]] -- [1,2,4,3,7,5,10,6,13,8,16,9,19,11,12,14,15,17,18,20]
-- filter (\x -> x `mod` 3 == 0) $ megaInterleave [[1,4..20], [2,5..20], [3,6..20]] -- [3,6,9,12,15,18]
-- filter (\x -> x `mod` 3 == 1) $ megaInterleave [[1,4..20], [2,5..20], [3,6..20]] -- [1,4,7,10,13,16,19]
-- filter (\x -> x `mod` 3 == 2) $ megaInterleave [[1,4..20], [2,5..20], [3,6..20]] -- [2,5,8,11,14,17,20]
megaInterleave :: [[a]] -> [a]
megaInterleave lists = foldr interleave [] lists
-- HINT: Use foldr

-- EXERCISE: Concatenate lists
-- EXAMPLES:
-- concat [[1,2,3],[4,5,6],[7,8,9]] -- [1,2,3,4,5,6,7,8,9]
concat :: [[a]] -> [a]
concat [] = []
concat ls = foldl (++) [] ls

---------------------------- SORTING

-- EXERCISE: Inserting into an ordered list
-- EXAMPLES:
-- insert 5 [1,7,10] -- [1,5,7,10]
-- insert 1337 [1,7,10] -- [1,7,10,1337]
-- insert 69 [] -- [69]
insert :: Integer -> [Integer] -> [Integer]
insert a [] = [a]
insert a (x:xs) = if a < x
                  then a : (x:xs)
                  else x : (insert a xs)

-- EXERCISE: Insertion sort
-- Sort a list by taking advantage of the following:
-- 0. [] is a sorted list
-- 1. If xs is a sorted list then insert x xs is also a sorted list
insertionSort :: [Integer] -> [Integer]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

-- EXERCISE: Merging lists
-- Merge two lists that are already sorted
-- merge [1,3,5] [2,4,6] -- merge [1,2,3,4,5,6]
-- merge [1,2,3] [4] -- [1,2,3,4]
merge :: [Integer] -> [Integer] -> [Integer]
merge [] l = l
merge l [] = l
merge (x1:xs1) (x2:xs2) = if x1 < x2
                          then x1 : (merge xs1 (x2:xs2))
                          else x2 : (merge (x1:xs1) xs2)

-- EXERCISE: Merge sort
-- Sort a list by taking advantage of the following:
-- 0. We can trivially sort a list with zero or one elements
-- 1. If we split a list in in half, and sort those two halves,
--    then we can use merge to create a new sorted list
mergeSort :: [Integer] -> [Integer]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort l = merge (mergeSort (take half l)) (mergeSort (drop half l))
          where half = quot (length l) 2

-- EXERCISE: Partitioning lists
-- Partition a list according to some predicate.
-- The resulting pair has the elements which satisfy the predicate as its first element,
-- and those that don't as its second element.
-- EXAMPLES:
-- partition even [1..10] -- ([2,4,6,8,10],[1,3,5,7,9])
-- partition (<5) [10,9..0] -- ([4,3,2,1,0],[10,9,8,7,6,5])
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition _ [] = ([],[])
partition f (x:xs) = if f x
                     then (x : (fst rec), (snd rec))
                     else (fst rec, (x: (snd rec)))
                 where rec = partition f xs

-- EXERCISE: Quicksort
-- Sort a list by taking advantage of the following:
-- If we take a number from a list, let's say p, we sort those smaller than p and those larger than p
-- and then we append these three in that order we will get a sorted list.
quickSort :: [Integer] -> [Integer]
quickSort [] = []
quickSort (x:xs) = (quickSort first) ++ [x] ++ (quickSort second)
              where part = partition (<x) xs
                    first = fst part
                    second = snd part

-- EXERCISE: Grouping
-- Group the numbers which are next to each other and equal to each other
-- group [] -- []
-- group [1,1,1,2,3] -- [[1,1,1],[2],[3]]
-- group [1,1,1,2,3,1,3,3] -- [[1,1,1],[2],[3],[1],[3,3]]
-- group [1..10] -- [[1],[2],[3],[4],[5],[6],[7],[8],[9],[10]]
group :: [Integer] -> [[Integer]]
group [] = []
group (x:xs) = equal : (group others)
          where part = partition (==x) xs
                equal = fst part ++ [x]
                others = snd part

-- EXERCISE: Taking from a list
-- Take elements from the beginning of a list while a predicate holds for them
-- EXAMPLES:
-- takeWhile even [1..10] -- []
-- takeWhile even [1,2,3] -- []
-- takeWhile even [2,4,5,6,8] -- [2,4]
-- takeWhile even [2,4,6,8] -- [2,4,6,8]
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x:xs) = if f x
                     then x : (takeWhile f xs)
                     else []

-- EXERCISE: Taking from a list
-- Drop elements from the beginning of a list while a predicate holds for them
-- EXAMPLES:
-- dropWhile even [] -- []
-- dropWhile even [1..10] -- [1,2,3,4,5,6,7,8,9,10]
-- dropWhile even [2,4,6,7,10,12] -- [7,10,12]
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile f (x:xs) = if f x
                     then dropWhile f xs
                     else x:xs

-- EXERCISE: Splitting a list
-- Split a string on a given character
-- Note that String in Haskell is simply [Char]
-- EXAMPLES:
-- splitOn 'a' "" -- []
-- splitOn 'a' "qwertya" --  ["qwerty"]
-- splitOn 'a' "aqwerty" -- ["","qwerty"]
-- splitOn 'a' "abracadabra" --  ["","br","c","d","br"]
myTail :: [a] -> [a]
myTail [] = []
myTail (_:xs) = xs

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn s l = (takeWhile (/=s) l) : splitOn s (myTail (dropWhile (/=s) l))
