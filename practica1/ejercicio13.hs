data AB a = Nil 
           | Bin (AB a) a (AB a)
--recordar

--version 1

foldrEJ :: (a -> b -> b) -> b -> [a] -> b
foldrEJ f z [] = z
foldrEJ f z (x:xs) = f x (foldrEJ f z xs) 

--version 2

foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2  f z lista = case lista of 
    [] -> z
    (x:xs) -> f x (rec xs)
    where rec = foldr2 f z 

--ahora fold para AB:

--version 1

foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB casoNil casoBin arbol = case arbol of
    Nil -> casoNil
    (Bin izq r der) -> casoBin (rec izq) r (rec der) 
    where rec = foldAB casoNil casoBin 

--version 2

foldAB2 :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB2 cNil _ Nil = cNil
foldAB2 cNil cBin (Bin hi r hd) = cBin (foldAB2 cNil cBin hd) r (foldAB2 cNil cBin hi)


-----------------------

--rec:

--para recAB quiero lo mismo solo que la f tome tambien a la "cola" , o sea digamos que tome el arbol izq y der

--recordar recr para listas.

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

--ahora recAB: 

--version 1

recAB :: (b -> AB a -> a -> AB a -> b -> b) -> b -> AB a -> b
recAB cBin cNil Nil = cNil
recAB cBin cNil (Bin hi r hd) = cBin (recAB cBin cNil hd) hd r hi (recAB cBin cNil hi)

--version 2

recAB2 :: b -> (AB a -> AB a -> b -> a -> b -> b) -> AB a -> b
recAB2 casoNil casoBin arbol = case arbol of
    Nil -> casoNil
    (Bin izq r der) -> casoBin izq der (rec izq) r (rec der) 
    where rec = recAB2 casoNil casoBin 



--CLAVE : cBin es f enrealidad, o sea es una funcion que toma un valor a y dos casos recursivos b, y devuelve un valor b. 


--o sea si llego a Nil, devuelvo el valor del caso base. 
--si estoy en caso Bin


esNil :: AB a -> Bool
esNil arbol = case arbol of
    Nil -> True 
    Bin izq r der -> False

altura :: AB a -> Integer
altura = foldAB 0 (\recizq r recder -> (max (recizq + 1) (recder + 1)))

cantNodos :: AB a -> Integer
cantNodos = foldAB 0 (\recizq r recder -> (1 + recizq + recder))

altura2 :: AB a -> Integer
altura2 Nil = 0
altura2 (Bin izq _ der) = 1 + max (altura2 izq) (altura2 der)

cantNodos2 :: AB a -> Integer
cantNodos2 Nil = 0
cantNodos2 (Bin izq _ der) = 1 + cantNodos2 izq + cantNodos2 der




mejorSegun :: a -> (a -> a -> Bool) -> AB a -> a
mejorSegun cNil f  = foldAB cNil (\hi r hd -> if (f hi hd && f hi r) then
                                                    hi else (if f hd r then hd else r)) 

-- como hacer mejorSegun preguntar.

{-


siempre tiene que haber la suma de as y bs , o sea en este caso, siemrpe tengo que tener 4 a y 4 b
8
abab --tengo 2 a y 2 b
aabb -- tengo 2 a y 2 b


abab -- tengo 2 a y 2 b
abbb -- tengo 1 a y 3 b --aca la cuenta esta mal, porque tengo 3 a y 5 b, asi que la proxima tengo que cambiar una b por una a.

abab 
abab

-}