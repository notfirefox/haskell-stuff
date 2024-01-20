import Data.Char

type Bit = Int

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

-- function to convert a sequence of bits to a decimal number
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

-- converts a integer to a bit sequence
oldInt2bin :: Int -> [Bit]
oldInt2bin n = n `mod` 2 : int2bin (n `div` 2)

-- chops a sequence of bits into a list of lists of 8 bits
oldChop8 :: [Bit] -> [[Bit]]
oldChop8 [] = []
oldChop8 bits = take 8 bits : oldChop8 (drop 8 bits)

-- unfold function
-- p = predicate(x)     (condition when to stop)
-- h = headFunction(x)  (transforms the head element)
-- t = traverse(x)      (changes the value x for traversal)
-- x = value
unfold p h t x
    | p x = []
    | otherwise = h x : unfold p h t (t x)

-- does the same as oldInt2bin
int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

-- appends 0s if length is less than zero
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- does the same oldChop8
chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

-- calculates the parity bit
parityBit :: [Bit] -> Bit
parityBit xs = length (filter (== 1) xs) `mod` 2

-- uses make8 but extends it with a parity bit
make9 :: [Bit] -> [Bit]
make9 bs = bits ++ [pbit]
  where
    bits = make8 bs
    pbit = parityBit bits

-- encodes a string as a bit sequence
encode :: String -> [Bit]
encode = concatMap (make9 . int2bin . ord)

-- same as chop8 but uses 9 bits and the 9th bit is a parity bit
chop9 :: [Bit] -> [[Bit]]
chop9 bs
    | null bits = []
    | cond = bits : chop9 (drop 9 bs)
    | otherwise = error "Parity bit mismatch."
  where
    bits = take 8 bs
    pbit = if length bs >= 9 then bs !! 8 else error "No parity bit."
    cond = parityBit bits == pbit

-- converts a bit sequence to a string
decode :: [Bit] -> String
decode = map (chr . bin2int) . chop9

-- channel function
channel :: [Bit] -> [Bit]
channel = id

-- faulty channel function
faultyChannel :: [Bit] -> [Bit]
faultyChannel = tail

-- transmit function
transmit :: String -> String
transmit = decode . channel . encode

-- custom map function
customMap :: (a -> b) -> [a] -> [b]
customMap f as = unfold null f' tail as
  where
    f' as = f (head as)

-- default iterator function
defaultIterate :: (a -> a) -> a -> [a]
defaultIterate f x = x : defaultIterate f (f x)

-- custom iterator function
customIterate :: (a -> a) -> a -> [a]
customIterate = unfold (const False) id

-- function that alternately applies two functions to two lists
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x : xs) = f x : altMap g f xs

-- doubles a number and subtracts 9 if it is greater than 9
luhnDouble :: Int -> Int
luhnDouble x = if t > 9 then t - 9 else t
  where
    t = x * 2

-- luhn algorithm for an arbitrary amount of digits
luhn :: [Int] -> Bool
luhn xs = sum (altMap luhnDouble id xs) `mod` 10 == 0

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

    -- int2bin function (expect: [1,0,1,1])
    putStrLn $ "oldInt2bin 13: " ++ show (oldInt2bin 13)
    putStrLn $ "int2bin 13: " ++ show (int2bin 13)

    -- make8 function
    putStrLn $ "make8 [1,0,1,1]: " ++ show (make8 [1, 0, 1, 1])

    -- bin2int function (expect: 13)
    let bits = [1, 0, 1, 1]
    putStrLn $ "bin2int [1,0,1,1]: " ++ show (bin2int bits)

    -- chop 8 function
    let bits = [1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1]
    putStrLn $ "oldChop8: " ++ show (oldChop8 bits)
    putStrLn $ "chop8: " ++ show (chop8 bits)

    -- parity bit function
    putStrLn $ "parityBit [1,0,0,1]: " ++ show (parityBit [1, 0, 0, 1])
    putStrLn $ "parityBit [1,1,0,1]: " ++ show (parityBit [1, 1, 0, 1])

    -- encode function
    putStrLn $ "encode `abc`: " ++ show (encode "abc")

    -- decode function
    let bits = [1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0]
    putStrLn $ "decode [...]: " ++ show (decode bits)

    -- transmit function
    let msg = "higher-order functions are easy"
    putStrLn $ "transmit `" ++ msg ++ "`: " ++ transmit msg

    -- custom map function
    let numbers5 = [1, 2, 3, 4, 5, 6]
    putStrLn $ "map odd [1,2,3,4,5,6]: " ++ show (map odd numbers)
    putStrLn $ "customMap odd [1,2,3,4,5,6]: " ++ show (customMap odd numbers)

    -- custom iterate function
    let result1 = take 10 (defaultIterate (+ 1) 0)
    let result2 = take 10 (customIterate (+ 1) 0)
    putStrLn $ "defaultIterate (+1) 0: " ++ show result1
    putStrLn $ "customIterate (+1) 0: " ++ show result2

    -- alt map function
    let result3 = altMap (+ 10) (+ 100) [0, 1, 2, 3, 4]
    putStrLn $ "altMap (+10) (+100) [0,1,2,3,4]: " ++ show result3

    -- luhn function
    putStrLn $ "luhn [1,7,8,4]: " ++ show (luhn [1, 7, 8, 4])
    putStrLn $ "luhn [4,7,8,3]: " ++ show (luhn [4, 7, 8, 3])
