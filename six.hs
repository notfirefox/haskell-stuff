-- factorial function that also works for negative numbers
fac :: Int -> Int
fac n
    | n < 0 = 0
    | n == 0 = 1
    | otherwise = n Prelude.* fac (n - 1)

-- sum down function
sumDown :: Int -> Int
sumDown 0 = 0
sumDown n = n + sumDown (n - 1)

-- define power as multiple multiplications
(^) :: Int -> Int -> Int
m ^ 0 = 1
m ^ n = m * (m Main.^ (n - 1))

-- euclid function
euclid :: Int -> Int -> Int
euclid 0 n = n
euclid m 0 = m
euclid n m
    | n < m = euclid n (m - n)
    | otherwise = euclid (n - m) n

-- decide if all logical values are true
and :: [Bool] -> Bool
and [b] = b
and (b : bs) = b && Main.and bs

-- concatenate a list of lists
concat :: [[a]] -> [a]
concat [] = []
concat [[]] = []
concat (x : xs) = x ++ Main.concat xs

-- produce a list with n identical elements
replicate :: Int -> a -> [a]
replicate 0 a = []
replicate n a = a : Main.replicate (n - 1) a

-- select the nth element of a list
(!!) :: [a] -> Int -> a
xs !! 0 = head xs
xs !! n = tail xs Main.!! (n - 1)

-- decide if a value is an element of a list
elem :: (Eq a) => a -> [a] -> Bool
elem _ [] = False
elem a (x : xs) = x == a || Main.elem a (tail xs)

-- recursive merge function
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
    | x < y = x : merge xs (y : ys)
    | otherwise = y : merge (x : xs) ys

-- merge sort function
msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [a] = [a]
msort xs = merge (msort left) (msort right)
  where
    left = fst halves
    right = snd halves
    halves = splitAt (length xs `div` 2) xs

-- sum up a list of numbers
sumNums :: (Num a) => [a] -> a
sumNums [] = 0
sumNums (n : ns) = n + sumNums ns

-- takes the n first elements from a list
takeElems :: Int -> [a] -> [a]
takeElems 0 _ = []
takeElems _ [] = []
takeElems n (x : xs) = x : takeElems (n - 1) xs

-- recursive way to get the last element of a list
lastElem :: (Num a) => [a] -> a
lastElem [a] = a
lastElem (x : xs) = lastElem xs

main = do
    -- factorial function
    putStrLn $ "fac 0: " ++ show (fac 0)
    putStrLn $ "fac 8: " ++ show (fac 3)
    putStrLn $ "fac (-1): " ++ show (fac (-1))

    -- sum down function
    putStrLn $ "sumDown 3: " ++ show (sumDown 3)

    -- multiplication 5 3
    putStrLn $ "2 ^ 3: " ++ show (2 Main.^ 3)

    -- euclid 6 27
    putStrLn $ "euclid 6 27: " ++ show (euclid 6 27)

    -- and function
    putStrLn $ "and True False True : " ++ show (Main.and [True, False, True])
    putStrLn $ "and True True: " ++ show (Main.and [True, True])

    -- concat function
    let numbers = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    putStrLn $ "concat " ++ show numbers ++ ": " ++ show (Main.concat numbers)

    -- replicate function
    putStrLn $ "replicate 3 True: " ++ show (Main.replicate 3 True)

    -- !! operator
    putStrLn $ "[2,4,8,16,32] !! 3: " ++ show ([2, 4, 8, 16, 32] Main.!! 3)

    -- elem function
    putStrLn $ "elem 8 [2,4,8,16]: " ++ show (Main.elem 8 [2, 4, 8, 16])

    -- merge function
    putStrLn $ "merge [2,5,6] [1,3,4]: " ++ show (merge [2, 5, 6] [1, 3, 4])

    -- merge sort function
    let unsorted = [1, 9, 0, 5, 6, 7, 8, 2, 4, 3]
    putStrLn $ "msort " ++ show unsorted ++ ": " ++ show (msort unsorted)

    -- sum nums function
    putStrLn $ "sumNums [1, 2, 3]: " ++ show (sumNums [1, 2, 3])

    -- take elements function
    putStrLn $ "takeElems 3 [1,2,3,4,5]: " ++ show (takeElems 3 [1, 2, 3, 4, 5])

    -- last element function
    putStrLn $ "lastElem [1,2,3,4]: " ++ show (lastElem [1, 2, 3, 4])
