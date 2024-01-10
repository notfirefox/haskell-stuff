import Data.Char

-- creates a grid using two numbers
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

-- creates a square excluding the dioganal
square :: Int -> [(Int, Int)]
square m = [(x, y) | (x, y) <- grid m m, x /= y]

-- replicates a value
replicateValue :: Int -> a -> [a]
replicateValue n x = [x | _ <- [1 .. n]]

-- find pythagorean triples
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- ws, y <- ws, z <- ws, cond x y z]
  where
    ws = [1 .. n]
    cond x y z = x ^ 2 + y ^ 2 == z ^ 2

-- calculates all the factors of a given number
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

-- calculates all the perfect numbers up to a certain number
perfect :: Int -> [Int]
perfect n = [x | x <- [1 .. n], sum (init (factors x)) == x]

-- returns the positions of a given element in a list
positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']

-- given a key k and a list of key value pairs (k', v)
-- it searches for all pairs where k == k'
find :: (Eq a) => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

-- returns the positions of a given element in a list
-- using the find function
positionsUsingFind :: (Eq a) => a -> [a] -> [Int]
positionsUsingFind x xs = find x (zip xs [0 ..])

-- calculates the scalar product of two lists
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct ys xs = sum [x * y | (x, y) <- zip ys xs]

-- calculates the count of a char in a string
countCaseSensitive :: Char -> String -> Int
countCaseSensitive x xs = length [x' | x' <- xs, x == x']

-- calculates the count of a char in a string
countCaseInsensitive :: Char -> String -> Int
countCaseInsensitive x xs = length [x' | x' <- xs, toLower x == toLower x']

-- returns the number of letters in a string
numOfLetters :: String -> Int
numOfLetters xs = length [x | x <- xs, isAlpha x]

-- converts a given letter to a number
let2int :: Char -> Int
let2int c = ord c - ord 'a'

-- convert an integer to a letter
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

-- shifts a given letter by a number
shift :: Int -> Char -> Char
shift n c
    | isLower c = int2let ((let2int c + n) `mod` 26)
    | isUpper c = toUpper (int2let ((let2int (toLower c) + n) `mod` 26))
    | otherwise = c

-- encodes a string using caesar cipher
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-- letter frequency table
table :: [Float]
table =
    [ 8.1
    , 1.5
    , 2.8
    , 4.2
    , 12.7
    , 2.2
    , 2.0
    , 6.1
    , 7.0
    , 0.2
    , 0.8
    , 4.0
    , 2.4
    , 6.7
    , 7.5
    , 1.9
    , 0.1
    , 6.0
    , 6.3
    , 9.0
    , 2.8
    , 1.0
    , 2.4
    , 0.2
    , 2.0
    , 0.1
    ]

-- calculate percentage
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

-- calculates the frequencies for the letters in a string
freqs :: String -> [Float]
freqs xs = [percent (countCaseInsensitive x xs) n | x <- ['a' .. 'z']]
  where
    n = numOfLetters xs

-- chi-square statistic
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

-- rotates a list by a given number
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- cracks the caesar cipher for a given string
crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
    table' = freqs xs

main = do
    -- sum of 100 first square numbers
    let sum1 = sum [x ^ 2 | x <- [1 .. 100]]
    putStrLn $ "Sum 1: " ++ show sum1

    -- grid 1 2
    putStrLn $ "Grid 1 2: " ++ show (grid 1 2)

    -- square 2
    putStrLn $ "Square 2: " ++ show (square 2)

    -- replicate 3 True
    putStrLn $ "Replicate 3 True: " ++ show (replicateValue 3 True)

    -- pyths 10
    putStrLn $ "Pyths 10: " ++ show (pyths 10)

    -- perfect 500
    putStrLn $ "Perfect 500: " ++ show (perfect 500)

    -- list comprehension and generators
    putStrLn "List comprehension and generators:"
    putStrLn $ "a) " ++ show [(x, y) | x <- [1, 2], y <- [3, 4]]
    let list1 = [(1, y) | y <- [3 .. 4]]
    let list2 = [(2, y) | y <- [3 .. 4]]
    putStrLn $ "b) " ++ show (list1 ++ list2)

    -- positions function
    let bools = [True, False, True, False]
    putStrLn "Positions function"
    putStrLn $ "a) " ++ show (positions False bools)
    putStrLn $ "a) " ++ show (positionsUsingFind False bools)

    -- scalar product
    let scalarResult = scalarProduct [1, 2, 3] [4, 5, 6]
    putStrLn $ "Scalar product [1,2,3] [4,5,6]: " ++ show scalarResult

    -- convert the letter c to an int
    putStrLn $ "let2int c: " ++ show (let2int 'c')

    -- convert an int to a letter
    putStrLn $ "int2let 2: " ++ show (int2let 2)

    -- (shift 3 a) and (shift (-3) c)
    putStrLn $ "Shift 3 a: " ++ show (shift 3 'a')
    putStrLn $ "Shift (-3) c: " ++ show (shift (-3) 'c')

    -- encode haskell is fun
    putStrLn "Encode 3 haskell is fun:"
    print (encode 3 "haskell is fun")

    -- percent 5 15
    putStrLn $ "Percent 5 15: " ++ show (percent 5 15)

    -- freqs abbcccddddeeeee
    putStrLn "Freqs abbcccddddeeeee"
    print (freqs "abbcccddddeeeee")

    -- rotate 3 [1,2,3,4,5]
    putStrLn $ "rotate 3 [1,2,3,4,5]: " ++ show (rotate 3 [1, 2, 3, 4, 5])

    -- crack "kdvnhoo lv ixq"
    putStrLn $ "crack \"kdvnhoo lv ixq\": " ++ show (crack "kdvnhoo lv ixq")

    -- crack "Vscd Mywzboroxcsyxc kbo ecopev"
    let secret = "Vscd Mywzboroxcsyxc kbo ecopev"
    putStrLn $ "crack \"" ++ secret ++ "\": " ++ show (crack secret)
