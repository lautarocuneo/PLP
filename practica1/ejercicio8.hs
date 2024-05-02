uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f (x,y) = f x y

mapPares :: (a -> b1 -> b2) -> [(a, b1)] -> [b2]
mapPares f = map (uncurry' f)


recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

armarPares :: [a] -> [b] -> [(a,b)]
armarPares [] _ = []
armarPares _ [] = []
armarPares (x:xs) (y:ys) = (x,y) : armarPares xs ys 




mizipwith :: (a -> b -> c) -> [a] -> [b] -> [c]
mizipwith _ [] _ = []
mizipwith _ _ [] = []
mizipwith f (x:xs) (y:ys) = (f x y) : (mizipwith f xs ys)

