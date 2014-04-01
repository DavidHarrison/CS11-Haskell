--
-- lab2.hs
--

-- Sum a list of integers recursively.
sumIntegers1 :: [Integer] -> Integer
sumIntegers1 [] = 0
sumIntegers1 (x:xs) = x + sumIntegers1 xs

-- Sum a list of integers in terms of foldl.
sumIntegers2 :: [Integer] -> Integer
sumIntegers2 xs = foldl (+) 0 xs

-- Product of a list of integers in terms of foldr.
prodIntegers :: [Integer] -> Integer
prodIntegers xs = foldr (*) 1 xs

-- List append in terms of foldr.
listAppend :: [a] -> [a] -> [a]
listAppend xs ys = foldr (:) ys xs

-- Insertion sort.
-- This sorts a list of values (which all have to be of type class Ord)
-- according to the following algorithm:
-- 1) Sort the sublist consisting of the tail of the list.
-- 2) Insert the first element (the head) into the sorted sublist at the
--    correct place.
-- Sort the list in ascending order.
insertionSort :: Ord a => [a] -> [a]
insertionSort [x] = [x]
insertionSort (x:xs) = insert x (insertionSort xs)
  where insert x [] = [x]
        insert x xs
          | x < head xs = x:xs
          | otherwise = (head xs):(insert x (tail xs))

-- 'map' that works on two lists.
-- map f a b = [(f a1 b1), (f a2 b2), ...]
--     where a = [a1, a2, ...] and b = [b1, b2, ...]
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ [] _ = []
map2 _ _ [] = []
map2 f (a:as) (b:bs) = (f a b):(map2 f as bs)

-- Infinite list of factorials, starting from 0!, using map2.
factorials :: [Integer]
factorials = 1:(map2 (*) factorials [2..])

-- Infinite list of prime numbers using list comprehensions and a sieving
-- algorithm. The sieve works as follows: it's n followed by (the sieve of) 
-- all numbers that aren't divisible by n.
primes :: [Integer]
primes = [x | x <- [2..], prime x]
  where prime x = indivisibleBelow (x - 1) x
        indivisibleBelow d x
          | d == 1 = True
          | (x `mod` d) == 0 = False
          | otherwise = indivisibleBelow (d - 1) x

-- Tree data structure.  Note that this is different from the Tree
-- structure used in the lecture.
data Tree a = Leaf | Node a (Tree a) (Tree a)
              deriving Show

-- Sample tree (for testing).
sampleTree :: Tree Integer
sampleTree = Node 1 (Node 2 (Node 3 Leaf Leaf) Leaf) (Node 4 Leaf Leaf)

-- Count the number of leaves in a Tree.
countLeaves :: Tree a -> Integer
countLeaves Leaf = 1
countLeaves (Node n t1 t2) = (countLeaves t1) + (countLeaves t2)

-- Mapping over a tree.
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Leaf = Leaf
mapTree f (Node a t1 t2) = Node (f a) (mapTree f t1) (mapTree f t2)

-- Some data/functions for testing:

-- Counting characters in a String.
countChars :: String -> Integer
countChars = toInteger . length

-- Sample tree of strings.
treeOfStrings :: Tree String
treeOfStrings = Node "foo" 
                (Node "bar" (Node "baz" Leaf Leaf) Leaf) 
                (Node "bam" Leaf Leaf)

treeOfNums :: Tree Integer
treeOfNums = mapTree countChars treeOfStrings


-- Fold on a tree.
foldTree :: (a -> a -> a) -> a -> Tree a -> a
foldTree _ z Leaf = z
foldTree f z (Node a t1 t2) = f a (foldTree f (foldTree f z t1) t2)

-- Sample use of foldTree: count the characters in a tree of Strings.
charsInTree :: Integer
charsInTree = foldTree (+) 0 (mapTree countChars treeOfStrings)


--
-- end of lab2.hs
--
