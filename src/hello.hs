main = do
    putStrLn "Hello, everybody!"
    let numbers = filter odd [10 .. 20]
    putStrLn ("Please look at my favorite odd numbers: " <> show numbers)
