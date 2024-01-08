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
