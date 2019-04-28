data Bintree a = Empty | Node a (Bintree a) (Bintree a)
    deriving (Show)

int_tree:: Bintree Integer
int_tree = Node 10 (Node 5 Empty Empty) Empty

insert:: (Ord a) => (Bintree a) -> a -> (Bintree a)
insert Empty x = Node x Empty Empty
insert (Node y l r) x | (x <= y) = Node y (insert l x) r
                      | (x > y) =  Node y l (insert r x)
                      
                      

insertList:: (Ord a) => (Bintree a) -> [a] -> (Bintree a)
insertList t [] = t
insertList t (x:xs) = insertList (insert t x) xs

a1:: Bintree Integer
a1 = Empty

a2::Bintree Integer
a2 = insertList a1 [-3..3]
