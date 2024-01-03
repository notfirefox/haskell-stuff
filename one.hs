-- faculty function
fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n - 1)

-- sum up a list of numbers
sumList :: (Num a) => [a] -> a
sumList [] = 0
sumList (n : ns) = n + sumList ns

productList :: (Num a) => [a] -> a
productList [] = 1
productList (n : ns) = n * productList ns

-- double the value of a given number
doubleNum :: (Num a) => a -> a
doubleNum a = 2 * a

-- quicksort
qsort :: [Integer] -> [Integer]
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

-- qsort but in reverse order
qsortrev :: [Integer] -> [Integer]
qsortrev [] = []
qsortrev (x : xs) = qsortrev larger ++ [x] ++ qsortrev smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

main = do
    -- faculty of 5
    let facResult = fac 5
    putStrLn $ "Faculty of 5: " ++ show facResult

    -- sum of a list of numbers
    let numbers = [1, 2, 3, 4, 5]
    let sumResult = sumList numbers
    putStrLn $ "Sum of the list [1 2,3,4,5]: " ++ show sumResult

    -- product of a list of numbers
    let otherNumbers = [2, 3, 4]
    let productResult = productList otherNumbers
    putStrLn $ "Product of the list [2,3,4]: " ++ show productResult

    -- double a number
    let x = 16
    let doubleResult = doubleNum x
    putStrLn $ "Double of 16: " ++ show doubleResult

    -- sort a list of numbers
    let unsorted = [3, 5, 1, 4, 2]
    let sorted = qsort unsorted
    putStrLn $ "Sorted List: " ++ show sorted

    -- sort a list in reversed order
    let sortedReverse = qsortrev unsorted
    putStrLn $ "Reverse Sorted List: " ++ show sortedReverse
