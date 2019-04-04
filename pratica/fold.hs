myFoldr :: (b -> a -> a) -> a -> [b] -> a

myFoldr op z [] = z
myFoldr op z ( x : xs ) = x `op` myFoldr op z xs


--myFoldl :: (c -> d -> c) -> c -> [d] -> c

--myFoldl op a [] = a -- a é acumulador
--myFoldl op a ( x : xs ) = myFoldl op (a `op` x) xs

--notas
-- myFoldr (:) [] = id
-- myFoldl (flip (:)) = reverse



