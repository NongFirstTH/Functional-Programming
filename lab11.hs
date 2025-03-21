data Tree a =
      Empty
    | Node (Tree a) a (Tree a)
  deriving (Show)

treeEmpty = Empty
treeA = Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty)
treeB = Node (Node Empty 1 Empty) 2 (Node Empty 3 (Node Empty 4 Empty))
treeC = Node (Node Empty 5 Empty) 2 (Node Empty 3 (Node Empty 4 Empty))

mapBT::(a->b) -> Tree a -> Tree b
mapBT _ Empty = Empty
mapBT f (Node l x r) = Node (mapBT f l) (f x) (mapBT f r)

foldBT :: (t1 -> t2 -> t1 -> t1) -> t1 -> Tree t2 -> t1
foldBT _ acc Empty = acc
foldBT f acc (Node l x r) = f (foldBT f acc l) x (foldBT f acc r)

heightBT :: (Num a1, Ord a1) => Tree a2 -> a1
heightBT Empty = 0
heightBT (Node l _ r) = max (heightBT l) (heightBT r) + 1

isBST :: (Ord t, Num t) => Tree t -> Bool
isBST tree = isBSTAux tree (-1) 100000

isBSTAux :: Ord t => Tree t -> t -> t -> Bool
isBSTAux Empty _ _ = True
isBSTAux (Node l x r) maxL minR =
    x > maxL && x < minR &&
    isBSTAux l maxL x &&
    isBSTAux r x minR

data NAryTree t = 
  NodeEmpty 
  | NodeNAry t [NAryTree t] 
  deriving (Show)

preorder :: NAryTree a -> [a]
preorder NodeEmpty = []
preorder (NodeNAry root child) = [root] ++ concatMap preorder child

postorder :: NAryTree a -> [a]
postorder NodeEmpty = []
postorder (NodeNAry root child) =
    concatMap postorder child ++ [root]
