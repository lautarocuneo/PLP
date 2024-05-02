data RoseTree a = Rose a [RoseTree a]
--no entiendo por que no tiene caso base.
--si tiene caso base no explicito , que es la lista vacia de rosetree.



foldRT :: (a -> [b] -> b) -> RoseTree a -> b
foldRT f (Rose x hijos) = f x (map (foldRT f) hijos)
--f es la funcion que vamos a aplicar que toma la "cabeza" y el caso recursivo. 


tamaño :: RoseTree a -> Int
tamaño (Rose x hijos) = 1 + sum (map tamaño hijos)


tamaño2 :: RoseTree a -> Int
tamaño2 = foldRT (\_ recs -> 1 + sum recs)
