-- Autumn Hale
-- CSCI 4230
-- Assignment 4

main = do
  putStrLn "Hi"

-- #1 returns the perimeter of a triangle
perimeter_of_rectangle :: Num a => a -> a -> a
perimeter_of_rectangle x y = x*2 + y*2

-- #2 checks if a number is even
is_even :: Integral a => a -> Bool
is_even x = ((mod x 2) == 0) 

-- #3 
mylength :: Integral b => [a] -> b
mylength [] = 0
mylength xs = 1 + mylength (tail xs)

-- #4 fibonocci
fib_if :: Integer -> Integer
fib_if n = if n == 0 || n == 1 then n else fib_if(n - 1) + fib_if(n - 2)

-- #5 fibonocci revised
fib_noif :: Integer -> Integer
fib_noif 0 = 0
fib_noif 1 = 1
fib_noif n = fib_noif(n - 1) + fib_noif(n - 2)

-- #6 reverses a list
reverseList :: [a] -> [a]
reverseList [] = []
reverseList xs = (last xs) : reverseList (init xs)

-- #7 
lower :: Ord a => a -> [a] -> [a]
lower n xs = [xs | xs <- xs, xs < n]

-- #8 quicksort 
higher :: Ord a => a -> [a] -> [a]
higher n xs = [xs | xs <- xs, xs > n || xs == n]

-- #9 quicksort recursive
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort (lower x xs) ++ [x] ++ quicksort (higher x xs)
 