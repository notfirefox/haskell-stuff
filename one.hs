-- faculty function
fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n - 1)

-- sum up a list of numbers
sumList :: (Num a) => [a] -> a
sumList [] = 0
sumList (n : ns) = n + sumList ns

-- double the value of a given number
doubleNum :: (Num a) => a -> a
doubleNum a = 2 * a

main = do
  -- faculty of 5
  let facResult = fac 5
  putStrLn $ "Faculty of 5: " ++ show facResult

  -- sum of a list of numbers
  let numbers = [1, 2, 3, 4, 5]
  let result = sumList numbers
  putStrLn $ "Sum of the list: " ++ show result

  -- double a number
  let x = 16
  let doubleResult = doubleNum x
  putStrLn $ "Double of 16: " ++ show doubleResult
