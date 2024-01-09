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

-- calculates the count of a char in a string
count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

-- returns the number of lowercase letters in a string
lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

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
freqs xs = [percent (count x xs) n | x <- ['a' .. 'z']]
  where
    n = lowers xs

main = do
    -- sum of 100 first square numbers
    let sum1 = sum [x ^ 2 | x <- [1 .. 100]]
    putStrLn $ "Sum 1: " ++ show sum1

    -- grid 1 2
    putStrLn $ "Grid 1 2: " ++ show (grid 1 2)

    -- square 2
    putStrLn $ "Square 2: " ++ show (square 2)

    -- replicate 3 True
    putStrLn $ "Replciate 3 True: " ++ show (replicateValue 3 True)

    -- pyths 10
    putStrLn $ "Pyths 10: " ++ show (pyths 10)

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
