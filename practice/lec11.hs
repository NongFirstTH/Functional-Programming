data Tree a = 
    Empty |
    Node (Tree a) a (Tree a)
    deriving (Show)

preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node l x r) = [x] ++ preorder l ++ preorder r

inorder Empty = []
inorder (Node l x r) =
    inorder l ++ [x] ++ inorder r

postorder Empty = []
postorder (Node l x r) =
    postorder l ++ postorder r ++ [x]

insert :: Ord t => Tree t -> t -> Tree t
insert Empty x = Node Empty x Empty
insert (Node l v r) x 
    | x < v = Node (insert l x) v r
    | otherwise = Node l v (insert r x)

contains :: Ord t => Tree t -> t -> Bool
contains Empty x = False
contains (Node l v r) x
    | x == v = True
    | x < v = contains l x
    | otherwise = contains r x

delete Empty x = Empty
delete (Node l v r) x
    | x < v = Node (delete l x) v r
    | x > v = Node l v (delete r x)