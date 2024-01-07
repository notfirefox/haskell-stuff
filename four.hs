-- takes a list of elements and returns two halves
halve :: [a] -> ([a], [a])
halve x = (take (length x `div` 2) x, drop (length x `div` 2) x)

-- returns the third element using head and tail
thirdHeadAndTail :: [a] -> a
thirdHeadAndTail xs = head (tail (tail xs))

-- returns the third element using list indexing
thirdListIndexing :: [a] -> a
thirdListIndexing xs = xs !! 2

-- returns the third element using pattern matching
thirdPatternMatching :: [a] -> a
thirdPatternMatching (_ : _ : (x : xs)) = x

-- returns the tail of a list using a conditinal expression
safeTailConditionalExpr :: [a] -> [a]
safeTailConditionalExpr xs =
    if null xs
        then []
        else drop 1 xs

-- returns the tail of a list using a guarded expression
safeTailGuardedEquation :: [a] -> [a]
safeTailGuardedEquation xs
    | null xs = []
    | otherwise = drop 1 xs

-- returns the tail of a list using pattern matching
safeTailPatternMatching :: [a] -> [a]
safeTailPatternMatching [] = []
safeTailPatternMatching (_ : xs) = xs

main = do
    putStrLn $ "Halve: " ++ show (halve [1, 2, 3, 4])

    let numbers = [2, 4, 8, 16, 32]
    putStrLn $ "Third entry of " ++ show numbers ++ ": "
    print (thirdHeadAndTail numbers)
    print (thirdListIndexing numbers)
    print (thirdPatternMatching numbers)

    putStrLn $ "SafeTail of " ++ show numbers ++ "; "
    print (safeTailConditionalExpr numbers)
    print (safeTailGuardedEquation numbers)
    print (safeTailPatternMatching numbers)

    let emptyList :: [Int] = []
    putStrLn $ "SafeTail of " ++ show emptyList ++ "; "
    print (safeTailConditionalExpr emptyList)
    print (safeTailGuardedEquation emptyList)
    print (safeTailPatternMatching emptyList)
