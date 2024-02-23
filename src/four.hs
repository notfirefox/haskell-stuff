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

-- disjunctor listing all of the cases explicitely
disjunctor1 :: Bool -> Bool -> Bool
disjunctor1 False False = False
disjunctor1 False True = True
disjunctor1 True False = True
disjunctor1 True True = True

-- disjunctor using only two cases
disjunctor2 :: Bool -> Bool -> Bool
disjunctor2 False False = False
disjunctor2 _ _ = True

-- another disjunctor using three cases
disjunctor3 :: Bool -> Bool -> Bool
disjunctor3 False b = b
disjunctor3 b False = b
disjunctor3 True True = True

-- another disjunctor using three cases
disjunctor4 :: Bool -> Bool -> Bool
disjunctor4 True _ = True
disjunctor4 _ True = True
disjunctor4 _ _ = False

-- conjunction without using only conditional expressions
conjunction1 :: Bool -> Bool -> Bool
conjunction1 b c = if b then if c then True else False else False

-- conjunction without using only conditional expressions
conjunction2 :: Bool -> Bool -> Bool
conjunction2 b c = if b then c else False

-- multiply using lambdas
mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x * y * z))

-- doubles a number and subtracts 9 if it is greater than 9
luhnDouble :: Int -> Int
luhnDouble x = if t > 9 then t - 9 else t
  where
    t = x * 2

-- luhn algorithm for four digits
luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (x + b + y + d) `mod` 10 == 0
  where
    x = luhnDouble a
    y = luhnDouble c

main = do
    -- halve a list into two parts
    putStrLn $ "Halve of [1, 2, 3, 4]: " <> show (halve [1, 2, 3, 4])

    -- third element of list
    let numbers = [2, 4, 8, 16, 32]
    putStrLn $ "Third entry of " <> show numbers <> ": "
    putStrLn $ "a) " <> show (thirdHeadAndTail numbers)
    putStrLn $ "b) " <> show (thirdListIndexing numbers)
    putStrLn $ "c) " <> show (thirdPatternMatching numbers)

    -- safetail
    putStrLn $ "SafeTail of " <> show numbers <> ": "
    putStrLn $ "a) " <> show (safeTailConditionalExpr numbers)
    putStrLn $ "b) " <> show (safeTailGuardedEquation numbers)
    putStrLn $ "c) " <> show (safeTailPatternMatching numbers)

    let emptyList :: [Int] = []
    putStrLn $ "SafeTail of " <> show emptyList <> ": "
    putStrLn $ "a) " <> show (safeTailConditionalExpr emptyList)
    putStrLn $ "b) " <> show (safeTailGuardedEquation emptyList)
    putStrLn $ "c) " <> show (safeTailPatternMatching emptyList)

    -- disjunction
    putStrLn "Disjunctor 1:"
    putStrLn $ "False || False: " <> show (disjunctor1 False False)
    putStrLn $ "False || True : " <> show (disjunctor1 False True)
    putStrLn $ "True  || False: " <> show (disjunctor1 True False)
    putStrLn $ "True  || True : " <> show (disjunctor1 True True)

    putStrLn "Disjunctor 2:"
    putStrLn $ "False || False: " <> show (disjunctor2 False False)
    putStrLn $ "False || True : " <> show (disjunctor2 False True)
    putStrLn $ "True  || False: " <> show (disjunctor2 True False)
    putStrLn $ "True  || True : " <> show (disjunctor2 True True)

    putStrLn "Disjunctor 3:"
    putStrLn $ "False || False: " <> show (disjunctor3 False False)
    putStrLn $ "False || True : " <> show (disjunctor3 False True)
    putStrLn $ "True  || False: " <> show (disjunctor3 True False)
    putStrLn $ "True  || True : " <> show (disjunctor3 True True)

    putStrLn "Disjunctor 4:"
    putStrLn $ "False || False: " <> show (disjunctor4 False False)
    putStrLn $ "False || True : " <> show (disjunctor4 False True)
    putStrLn $ "True  || False: " <> show (disjunctor4 True False)
    putStrLn $ "True  || True : " <> show (disjunctor4 True True)

    -- conjunction
    putStrLn "Conjunction 1:"
    putStrLn $ "False || False: " <> show (conjunction1 False False)
    putStrLn $ "False || True : " <> show (conjunction1 False True)
    putStrLn $ "True  || False: " <> show (conjunction1 True False)
    putStrLn $ "True  || True : " <> show (conjunction1 True True)

    putStrLn "Conjunction 2:"
    putStrLn $ "False || False: " <> show (conjunction2 False False)
    putStrLn $ "False || True : " <> show (conjunction2 False True)
    putStrLn $ "True  || False: " <> show (conjunction2 True False)
    putStrLn $ "True  || True : " <> show (conjunction2 True True)

    -- multiplication
    putStrLn $ "Mult(2, 3, 4): " <> show (mult 2 3 4)

    -- luhn algorithm
    putStrLn $ "luhnDouble 3: " <> show (luhnDouble 3)
    putStrLn $ "luhnDouble 6: " <> show (luhnDouble 6)
    putStrLn $ "luhn(1, 7, 8, 4): " <> show (luhn 1 7 8 4)
    putStrLn $ "luhn(4, 7, 8, 3): " <> show (luhn 4 7 8 3)
