
--redefinicion de suma
sumaLista :: [Integer] -> Integer
sumaLista  = foldr (+) 0 


-- Redefinición de elem

miElem :: (Foldable t, Eq a) => a -> t a -> Bool
miElem x = foldr (\y valorcola -> (x == y) || valorcola) False

miConcat :: Foldable t => t a -> [a] -> [a]
miConcat xs ys = foldr (:) ys xs

miFilter :: Foldable t => (a -> Bool) -> t a -> [a]
miFilter p = foldr (\x xs -> if p x then x : xs else xs) []

miMap :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
miMap f = foldr (\x xs -> f x : xs) []


mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x y -> if f x y then x  else y) 

maximo :: Ord a => [a] -> a
maximo = mejorSegun (>)


sumasParciales :: [Integer] -> [Integer]
sumasParciales  = foldl (\acc x -> (head acc + x) : acc) [0] 

--el acumulador empieza valiendo [0], o sea la lista con un cero, y voy  a la cabeza de la lista acumuladora, 
--sumandole x (que es el elemento actual de la lista.)  y agregandola al acumulador.
--al final hay que pedir un reverse porque va a quedar al reves la lista si no.

sumasAlt :: [Integer] -> Integer
sumasAlt = foldr (-) 0
--[1,2,3] 1 - (2 - (3 - 0))

-- este no se como es

sumasAltFoldl :: [Integer] -> Integer
sumasAltFoldl = foldl (-) 0

-- [1,2,3] 0 - 1 - ()