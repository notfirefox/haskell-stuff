-- calculates the average of a list of integers
average :: [Int] -> Int
average ns = sum ns `div` length ns

-- another way to get the last element of a list
last1 :: (Num a) => [a] -> a
last1 xs = xs !! (length xs - 1)

-- another way to get the last element of a list
last2 :: (Num a) => [a] -> a
last2 [a] = a
last2 (x : xs) = last2 xs

-- another way to remove the last element from a list
init1 :: (Num a) => [a] -> [a]
init1 [a] = []
init1 xs = take (length xs - 1) xs

-- another way to remove the last element from a list
init2 :: (Num a) => [a] -> [a]
init2 [x] = []
init2 (x : xs) = x : init2 xs

main = do
    -- list of numbers
    let numbers :: [Int] = [1, 2, 3, 4, 5]
    putStrLn $ "List of numbers: " <> show numbers

    -- head of list
    putStrLn $ "Head: " <> show (head numbers)

    -- tail of list
    putStrLn $ "Tail: " <> show (tail numbers)

    -- length of a list
    putStrLn $ "Length: " <> show (length numbers)

    -- average of the list of numbers
    putStrLn $ "Average: " <> show (average numbers)

    -- element at index of list
    putStrLn $ "Element at index 2: " <> show (numbers !! 2)

    -- take the first three elements
    putStrLn $ "Take the first three elements: " <> show (take 3 numbers)

    -- remove the first three elements
    putStrLn $ "Drop the first three elements: " <> show (drop 3 numbers)

    -- reverse the list
    putStrLn $ "Reverse the list: " <> show (reverse numbers)

    -- append another list
    let otherNumbers = [6, 7, 8, 9, 10]
    putStrLn $ "Append another list: " <> show (numbers <> otherNumbers)

    -- last element
    putStrLn "Get the last element of a list:"
    print (Prelude.last numbers)
    print (last1 numbers)
    print (last2 numbers)

    -- remove last element
    putStrLn "Remove the last element of a list:"
    print (Prelude.init numbers)
    print (init1 numbers)
    print (init2 numbers)
