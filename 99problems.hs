------------------------- Problem 1 to 10 --------------------------------------

-- Problem 1
myLast :: [a] -> a
myLast [last] = last
myLast (first:rest) = myLast rest

-- Problem 2
myButLast :: [a] -> a
myButLast [butLast, _] = butLast
myButLast (first:rest) = myButLast rest

-- Problem 3
elementAt :: [a] -> Integer -> a
elementAt (first:rest) 1 = first
elementAt (first:rest) index = elementAt rest (index - 1)

-- Problem 4
myLength :: [a] -> Integer
myLength [item] = 1
myLength (first:rest) = myLength [first] + myLength rest

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (first:rest) = (myReverse rest) ++ [first]

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [character] = True
isPalindrome (first:rest) = first == myLast rest && isPalindrome (init rest)

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: (NestedList x) -> [x]
flatten (Elem x) = [x]
flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))
flatten (List []) = []

-- Problem 8
compress :: Eq a => [a] -> [a]
compress [x] = [x]
compress list
  | head list == (head . tail) list = (compress . tail) list
  | otherwise = (head list):((compress . tail) list)

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack [x] = [[x]]
pack list
  | head list == (head . tail) list =
      (head list : (head . pack . tail) list) : (tail . pack . tail) list
  | otherwise = pack [(head list)] ++ (pack . tail) list

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode list = map (\list -> (length list, head list)) (pack list)

------------------------- Problem 11 to 20 -------------------------------------

-- Problem 11
data Frequency item = Single item | Multiple Int item deriving (Show)
encodeModified :: Eq a => [a] -> [Frequency a]
encodeModified list =
  map (\i -> case i of Multiple 1 x -> Single x; i -> i) $
    map (\(i, x) -> Multiple i x) $ encode list

-- Problem 12
decodeModified :: [Frequency a] -> [a]
decodeModified [(Single item)] = [item]
decodeModified [(Multiple 1 item)] = decodeModified [(Single item)]
decodeModified [(Multiple c i)] = [i] ++ decodeModified [(Multiple (c-1) i)]
decodeModified (x:xs) = decodeModified [x] ++ decodeModified xs

-- Problem 13
encodeDirect :: (Eq a) => [a] -> [Frequency a]
encodeDirect [] = []
encodeDirect [a] = [Single a]
encodeDirect (x:y:xs)
  | x == y = encodeDirect' (Multiple 1 x) (y:xs)
  | otherwise = (Single x) : encodeDirect (y:xs)
  where encodeDirect' (Multiple c x) [] = [(Multiple c x)]
        encodeDirect' (Multiple c y) (x:xs)
          | y == x = encodeDirect' (Multiple (c+1) y) xs
          | otherwise = (Multiple c y) : encodeDirect (x:xs)

-- Problem 14
-- | Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli [] = []
dupli (x:yz) = x:x : dupli yz

-- Problem 15
-- | Replicate the items of a list a given number of times.
repli :: [a] -> Integer -> [a]
repli [] i = []
repli l 0 = []
repli (x:yz) i = (x : repli [x] (i-1)) ++ repli yz i

-- Problem 16
-- | Drop every n'th element from a list
drop :: [a] -> Integer -> [a]
drop xs n = [x | (x, i) <- zip xs (cycle [1..n]), i /= n]

-- Problem 17
-- | Split a list into two parts; the length of the first part is given.
split :: [a] -> Int -> [[a]]
split xs l = [[x | (x, i) <- t, i <= l], [y | (y, j) <- t, j > l]]
             where t = zip xs [1..n]
                   n = length xs

-- Problem 18
-- | Extract a slice from a list.
slice :: [a] -> Int -> Int -> [a]
slice xs i j = slice' xs i (j+1-i)
               where slice' xs 1 0 = []
                     slice' (x:xs) 1 j = x : (slice' xs 1 (j-1))
                     slice' (x:xs) i j = slice' xs (i-1) j

-- Problem 19
-- | Rotate a list n places to the left.
rotate :: [a] -> Integer -> [a]
rotate xs r
  | r > 0  = let xs' = tail xs ++ [head xs] in rotate xs' (r-1)
  | r == 0 = xs
  | r < 0  = let xs' = last xs : init xs in rotate xs' (r+1)

-- Problem 20
-- | Remove the K'th element from a list.
removeAt :: Int -> [a] -> [[a]]
removeAt i xs = [[xs !! (i-1)], [x | (x, j) <- t, j /= i]]
                where t = zip xs [1..n]
                      n = length xs

------------------------- Problem 21 to 30 --------------------------------

-- Problem 21
-- | Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs i = [y | (y, j) <- t, j < i] ++ [x] ++ [z | (z, k) <-t, k >= i]
                  where t = zip xs [1..n]
                        n = length xs

-- Problem 22
-- | Create a list containing all integers within a given range.
range :: Integer -> Integer -> [Integer]
range min max = [min..max]

-- Problems 23, 24, 25 use random monad. So I'll skip them for now.

-- Problem 26
-- | Generate the combinations of K distinct objects chosen from the N
-- | elements of a list.
combinations :: Int -> [a] -> [[a]]
combinations i xs = map (\xs -> map value xs) $ combinations' i $ number xs
                    where value (index, value) = value
                          combinations' 1 xs = [[x] | x <- xs]
                          combinations' i xs = [y:ys | y <- xs
                                                     , ys <- (combinations' (i-1) xs)
                                                     , y `before` ys]
                          before x [] = True
                          before x (y:ys) = position x < position y && before x ys
                          position (index, value) = index
                          number xs = zip [1..] xs

-- Problem 27
-- | Group the elements of a set into disjoint subsets.
group :: Eq a => [Int] -> [a] -> [[[a]]]
group [] xs = [[]]
group (i:is) xs = [y:ys | y <- combinations i xs, ys <- group is (xs `without` y)]
                  where without xs [] = xs
                        without xs (y:ys) = [x | x <- xs, x /= y] `without` ys

-- Problem 28
-- | a) Sort a list of list according to the length of the sublists.
lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) =    lsort [a | a <- xs, length a < length x]
               ++ [x]
               ++ lsort [b | b <- xs, length b >= length x]
-- | b) Sort the elements of this list according to their length frequency.
lfsort :: [[a]] -> [[a]]
lfsort xs = map (\x -> snd x) $ lfsort' $ map (\x -> ((freq x xs), x)) xs
            where freq x xs = length [y | y <- xs, length y == length x]
                  lfsort' [] = []
                  lfsort' (x:xs) =    lfsort' [y | y <- xs, fst y < fst x]
                                   ++ [x]
                                   ++ lfsort' [y | y <- xs, fst y >= fst x]

-- Problem 31
-- | Determin if a number is prime.
isprime :: Integer -> Bool
isprime 1 = True
isprime 2 = True
isprime 3 = True
isprime i = not $ divisors `divide` i
            where divisors = takeWhile (\x -> x*x <= i) $ 2:[3,5..]
                  divide [] i = False
                  divide (x:xs) i = i `mod` x == 0 || xs `divide` i

-- Problem 32
-- | Determine the greatest common divisor of two positive integer numbers.
-- | Use Euclid's algorithm.
myGCD :: Integer -> Integer -> Integer
myGCD x 0 = x
myGCD x y = myGCD y (x `mod` y)

-- Problem 33
-- | Determine whether two positive integer numbers are coprime.
-- | Two numbers are coprime if their greatest common divisor equals 1.
coprime :: Integer -> Integer -> Bool
coprime x y = myGCD x y == 1

-- Problem 34
-- | Calculate Euler's totient function phi(m).
totient :: Integer -> Integer
totient x = totient' x (x-1) 0
            where totient' x 0 n = n
                  totient' x i n
                    | coprime x i = totient' x (i-1) (n+1)
                    | otherwise   = totient' x (i-1) n

-- Problem 35
-- | Determine the prime factors of a given positive integer.
primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors x = primeFactors' x $ 2:[3,5..]
                 where primeFactors' x (y:ys)
                         | isprime y && x `mod` y == 0 = y : (primeFactors $ div x y)
                         | otherwise = primeFactors' x ys

-- Problem 36
-- | Construct a list containing the prime factors and their multiplicity.
primeFactors' :: Integer -> [(Integer, Integer)]
primeFactors' x = map (\(a,b) -> (b,toInteger a)) $ encode $ primeFactors x

-- Problem 37
-- | Calculate Euler's totient function phi(m) (improved).
totient' :: Integer -> Integer
totient' x = product [(p-1) * p ^ (m-1) | (p,m) <- primeFactors' x]

-- Problem 39
primesR :: Integer -> Integer -> [Integer]
primesR a b
  | even a = filter isprime [a+1, a+3..b]
  | otherwise = filter isprime [a, a+2..b]

-- Problem 40
-- | Write a predicate to find the two prime numbers that sum
-- | up to a given even integer.
goldbach :: Integer -> (Integer, Integer)
goldbach x = goldbach' 2 x
             where goldbach' i x
                     | isprime i && isprime (x-i) = (i, x-i)
                     | otherwise = goldbach' (i+1) x

-- Problem 41
-- | Given a range of integers by its lower and upper limit, print a list
-- | of all even numbers and their Goldbach composition.
goldbachList :: Integer -> Integer -> [(Integer, Integer)]
goldbachList a b = map goldbach (area [2,4..] a b)
                   where area (x:xs) a b
                           | x < a = area xs a b
                           | x > b = []
                           | otherwise = x : area xs a b
