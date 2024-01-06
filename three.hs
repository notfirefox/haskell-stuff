bools :: [Bool]
bools = [True, True, False]

nums :: [[Int]]
nums = [[0], [1, 2], [1, 2, 3]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy a = (a, a)

apply :: (a -> b) -> a -> b
apply f a = f a

second :: [x] -> x
second xs = head (tail xs)

swap :: (x, y) -> (y, x)
swap (x, y) = (y, x)

pair :: x -> y -> (x, y)
pair x y = (x, y)

double :: (Num x) => x -> x
double x = x * 2

palindrome :: (Eq x) => [x] -> Bool
palindrome xs = reverse xs == xs

twice :: (x -> x) -> x -> x
twice f x = f (f x)

main = do
    putStrLn "Hello"
    putStrLn $ "Add 2, 3, 4: " ++ show (add 2 3 4)
    putStrLn $ "Copy 4: " ++ show (copy 4)
    putStrLn $ "Apply copy 8: " ++ show (apply copy 8)
    putStrLn $ "Second [1, 2, 3]: " ++ show (second [1, 2, 3])
    putStrLn $ "Swap (2, 4): " ++ show (swap (2, 4))
    putStrLn $ "Pair 1, 2: " ++ show (pair 1 2)
    putStrLn $ "Double 16: " ++ show (double 16)
    putStrLn $ "Palindrome anna: " ++ show (palindrome "anna")
    putStrLn $ "twice second 2: " ++ show (twice double 2)
