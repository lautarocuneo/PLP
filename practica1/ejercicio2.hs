
--le doy una funcion de dos argumentos y me devuelve la funcion con
curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x,y)

--lo que hace esto es que le pasamos una funcion f a curry, y se queda a la espera de los dos parametros, ahi esta currificada cuando pasa eso. 


miUncurry :: (a -> b -> c) -> (a, b) -> c
miUncurry f (x,y) = f x y

--ejemplo de funcionamiento de uncurry.
-- Definimos una función que toma dos argumentos y devuelve la suma de ellos
suma :: Int -> Int -> Int
suma x y = x + y

-- Ahora usamos uncurry para convertir suma en una función que toma una tupla
sumaTupla :: (Int, Int) -> Int
sumaTupla = miUncurry suma

-- Ahora podemos usar sumaTupla con una tupla
resultado = sumaTupla (3, 4)  -- resultado es 7


--3) NO NO SE PUEDE NO TIPA.

