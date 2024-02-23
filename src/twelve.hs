-- tree data type
data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Show)

-- implement functor for tree
instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g Leaf = Leaf
    fmap g (Node l a r) = Node (fmap g l) (g a) (fmap g r)

main = do
    let tree = Node (Node Leaf 2 Leaf) 7 (Node Leaf 8 Leaf)
    putStrLn $ "Length: " ++ show (fmap even tree)
