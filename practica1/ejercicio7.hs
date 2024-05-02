-- Parte i
genLista :: a -> (a -> a) -> Integer -> [a]
genLista x f n
  | n <= 0    = []
  | otherwise = x : genLista (f x) f (n-1)

-- Parte ii
desdeHasta :: Integer -> Integer -> [Integer]
desdeHasta x y = genLista x (+1) (y - x + 1)