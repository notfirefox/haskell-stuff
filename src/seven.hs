-- apply map and filter using list comprehension
mapAndFilter :: (x -> y) -> (x -> Bool) -> [x] -> [y]
mapAndFilter f p (x : xs) = [f x | x <- xs, p x]

-- returns true if the predicate fn is true for all elements
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

-- returns true if the predicate fn is true for any element
any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

-- takes elements from a list while a predicate is true
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x : xs)
    | p x = x : Main.takeWhile p xs
    | otherwise = []

-- takes elements from a list while a predicate is true
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x : xs)
    | p x = Main.dropWhile p xs
    | otherwise = x : xs

-- map function using foldr
mapFoldr :: (x -> y) -> [x] -> [y]
mapFoldr f = foldr (#) []
  where
    x # ys = f x : ys

-- filter function using foldr
filterFoldr :: (x -> Bool) -> [x] -> [x]
filterFoldr p = foldr (#) []
  where
    x # ys = if p x then x : ys else ys

-- e.g. [2,3,4,5] -> 2345
dec2int :: [Int] -> Int
dec2int = foldl (#) 0
  where
    x # y = (x * 10) + y

-- curry function
curry :: ((a, b) -> c) -> (a -> b -> c)
curry f x y = f (x, y)

-- uncurry function
uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (x, y) = f x y

main = do
    -- map and filter
    let numbers = [1, 2, 3, 4, 5, 6, 7, 8]
    let result1 = mapAndFilter (+ 10) even numbers
    let result2 = filter even (map (+ 10) numbers)
    putStrLn $ "Result 1: " ++ show result1
    putStrLn $ "Result 2: " ++ show result2

    -- all function
    putStrLn $ "all even [1,2,3,4,5,6,7,8] : " ++ show (Main.all even numbers)
    putStrLn $ "all even [2,4,6,8] : " ++ show (Main.all even [2, 4, 6, 8])

    -- any function
    putStrLn $ "any even [1,2,3,4,5,6,7,8] : " ++ show (Main.any even numbers)
    putStrLn $ "any even [1,3,7,9] : " ++ show (Main.any even [1, 3, 7, 9])

    -- take while and drop while function
    let numbers1 = [1, 3, 2, 4]
    putStrLn $ "takeWhile odd [1,3,2,4]: " ++ show (Main.takeWhile odd numbers1)
    putStrLn $ "dropWhile odd [1,3,2,4]: " ++ show (Main.dropWhile odd numbers1)

    -- mapFoldr function
    let numbers2 = [1, 2, 3]
    putStrLn $ "mapFoldr (*2) [1,2,3]: " ++ show (mapFoldr (2 *) numbers2)

    -- filterFoldr function
    let numbers3 = [1, 2, 3, 4]
    putStrLn $ "filterFoldr odd [1,2,3,4]: " ++ show (filterFoldr odd numbers3)

    -- dec2int function
    let numbers4 = [2, 3, 4, 5]
    putStrLn $ "dec2int [2,3,4,5]: " ++ show (dec2int numbers4)
