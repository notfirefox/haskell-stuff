-- simple type definition
type String = [Char]

-- position type with two values
type Pos = (Int, Int)

-- pair type with type a
type Pair a = (a, a)

-- type declaration with two parameters
type Assoc k v = [(k, v)]

-- generic find function
find :: (Eq k) => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

-- bool data type
data FalseOrTrue = ItsFalse | ItsTrue

-- move data type
data Move = North | South | East | West

-- given a move and a position is returns a new position
move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y - 1)
move East (x, y) = (x + 1, y)
move West (x, y) = (x - 1, y)

-- shape data type
data Shape = Circle Float | Rect Float Float

-- calculates the area of a shape
area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y

-- recursive type for natural numbers
data Nat = Zero | Succ Nat

-- converts a nat to an int
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

-- converts a int to a natural number
int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

-- adds together two natual numbers
add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

-- multiplies two natural numbers
mult :: Nat -> Nat -> Nat
mult (Succ Zero) n = n
mult (Succ m) n = add (mult m n) n

-- power function for natural numbers
pow :: Nat -> Nat -> Nat
pow m Zero = Succ Zero
pow m (Succ n) = mult m (pow m n)

-- tree data type
data Tree a where
    Leaf :: a -> Tree a
    Node :: (Tree a) -> a -> (Tree a) -> Tree a

-- returns if a value is present in a search tree
occurs :: (Ord a) => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = case compare x y of
    LT -> occurs x l
    GT -> occurs x r
    EQ -> True

-- another tree data type
-- this tree only has data in its leaves
data Tree2 a where
    Leaf2 :: a -> Tree2 a
    Node2 :: (Tree2 a) -> (Tree2 a) -> Tree2 a

-- returns the number of leaves for a tree
leaves :: Tree2 a -> Int
leaves (Leaf2 _) = 1
leaves (Node2 l r) = leaves l + leaves r

-- decides if a binary tree is balanced or not
balanced :: Tree2 a -> Bool
balanced (Leaf2 _) = True
balanced (Node2 l r) =
    abs (leaves l - leaves r) <= 1
        && balanced l
        && balanced r

-- turns a non empty list into a balanced tree
balance :: [a] -> Tree2 a
balance [x] = Leaf2 x
balance xs = Node2 (balance l) (balance r)
  where
    (l, r) = splitAt (length xs `div` 2) xs

main = do
    -- add and mult function
    let three = int2nat 3
    let twelve = int2nat 12
    putStrLn $ "add three twelve: " ++ show (nat2int (add three twelve))
    putStrLn $ "mult three twelve: " ++ show (nat2int (mult three twelve))

    -- pow function
    let two = int2nat 2
    let ten = int2nat 10
    putStrLn $ "pow two ten: " ++ show (nat2int (pow two ten))

    -- occurs function
    let tree = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))
    putStrLn $ "occurs 4 tree: " ++ show (occurs 4 tree)

    -- balanced function
    let tree2 = Node2 (Node2 (Leaf2 1) (Leaf2 4)) (Node2 (Leaf2 6) (Leaf2 9))
    putStrLn $ "balanced tree2: " ++ show (balanced tree2)
    let tree3 = Node2 (Node2 (Node2 (Leaf2 1) (Leaf2 2)) (Node2 (Leaf2 3) (Leaf2 4))) (Leaf2 7)
    putStrLn $ "balanced tree3: " ++ show (balanced tree3)

    -- balance function
    let list :: [Int] = [1, 2, 3, 4, 5]
    let balancedTree = balance list
    putStrLn $ "balanced balance [1,2,3,4,5]: " ++ show (balanced balancedTree)