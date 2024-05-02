import Data.Either
import Data.List

data Dirección = Norte | Sur | Este | Oeste
  deriving (Eq, Show)
type Posición = (Float, Float)

data Personaje = Personaje Posición String  -- posición inicial, nombre
  | Mueve Personaje Dirección               -- personaje que se mueve, dirección en la que se mueve
  | Muere Personaje                         -- personaje que muere
  deriving (Eq, Show)
data Objeto = Objeto Posición String        -- posición inicial, nombre
  | Tomado Objeto Personaje                 -- objeto que es tomado, personaje que lo tomó
  | EsDestruido Objeto                      -- objeto que es destruido
  deriving (Eq, Show)
type Universo = [Either Personaje Objeto]

siguiente_posición :: Posición -> Dirección -> Posición
siguiente_posición p Norte = (fst p, snd p + 1)
siguiente_posición p Sur = (fst p, snd p - 1)
siguiente_posición p Este = (fst p + 1, snd p)
siguiente_posición p Oeste = (fst p - 1, snd p)

posición :: Either Personaje Objeto -> Posición
posición (Left p) = posición_personaje p
posición (Right o) = posición_objeto o

posición_objeto :: Objeto -> Posición
posición_objeto = foldObjeto const (const posición_personaje) id

nombre :: Either Personaje Objeto -> String
nombre (Left p) = nombre_personaje p
nombre (Right o) = nombre_objeto o

nombre_personaje :: Personaje -> String
nombre_personaje = foldPersonaje (const id) const id

está_vivo :: Personaje -> Bool
está_vivo = foldPersonaje (const (const True)) (const (const True)) (const False)

fue_destruido :: Objeto -> Bool
fue_destruido = foldObjeto (const (const False)) const (const True)

universo_con :: [Personaje] -> [Objeto] -> [Either Personaje Objeto]
universo_con ps os = map Left ps ++ map Right os

es_un_objeto :: Either Personaje Objeto -> Bool
es_un_objeto (Left o) = False
es_un_objeto (Right p) = True

es_un_personaje :: Either Personaje Objeto -> Bool
es_un_personaje (Left o) = True
es_un_personaje (Right p) = False

-- Asume que es un personaje
personaje_de :: Either Personaje Objeto -> Personaje
personaje_de (Left p) = p

-- Asume que es un objeto
objeto_de :: Either Personaje Objeto -> Objeto
objeto_de (Right o) = o

en_posesión_de :: String -> Objeto -> Bool
en_posesión_de n = foldObjeto (const (const False)) (\ r p -> nombre_personaje p == n) (const False)

objeto_libre :: Objeto -> Bool
objeto_libre = foldObjeto (const (const True)) (const (const False)) (const False)

norma2 :: (Float, Float) -> (Float, Float) -> Float
norma2 p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)

cantidad_de_objetos :: Universo -> Int
cantidad_de_objetos = length . objetos_en

cantidad_de_personajes :: Universo -> Int
cantidad_de_personajes = length . personajes_en

distancia :: (Either Personaje Objeto) -> (Either Personaje Objeto) -> Float
distancia e1 e2 = norma2 (posición e1) (posición e2)

{- objetos_libres_en :: Universo -> [Objeto]
objetos_libres_en u = filter objeto_libre (objetos_en u) -}

está_el_personaje :: String -> Universo -> Bool
está_el_personaje n = foldr (\x r -> es_un_personaje x && nombre x == n && (está_vivo $ personaje_de x) || r) False

está_el_objeto :: String -> Universo -> Bool
está_el_objeto n = foldr (\x r -> es_un_objeto x && nombre x == n && not (fue_destruido $ objeto_de x) || r) False

-- Asume que el personaje está
{- personaje_de_nombre :: String -> Universo -> Personaje
personaje_de_nombre n u = foldr1 (\x1 x2 -> if nombre_personaje x1 == n then x1 else x2) (personajes_en u) -}

-- Asume que el objeto está
{- objeto_de_nombre :: String -> Universo -> Objeto
objeto_de_nombre n u = foldr1 (\x1 x2 -> if nombre_objeto x1 == n then x1 else x2) (objetos_en u) -}

es_una_gema :: Objeto -> Bool
es_una_gema o = isPrefixOf "Gema de" (nombre_objeto o)

{-Ejercicio 1-}

foldPersonaje :: (Posición -> String -> a) -> (a -> Dirección -> a) -> (a -> a) -> Personaje -> a
foldPersonaje fPersonaje fMueve fMuere p = case p of
  Personaje pos nombre -> fPersonaje pos nombre
  Mueve pj dire -> fMueve (rec pj) dire
  Muere pj -> fMuere (rec pj)
  where rec = foldPersonaje fPersonaje fMueve fMuere

foldObjeto :: (Posición -> String -> a) -> (a -> Personaje -> a) -> (a -> a) -> Objeto -> a
foldObjeto fObjeto fTomado fEsDestruido o = case o of
  Objeto pos nombre -> fObjeto pos nombre
  Tomado ob pj        -> fTomado (rec ob) pj
  EsDestruido ob       -> fEsDestruido (rec ob) 
  where rec = foldObjeto fObjeto fTomado fEsDestruido

{-Ejercicio 2-}

posición_personaje :: Personaje -> Posición
{- posición_personaje = foldPersonaje (\pos nombre -> pos) (\pos dire -> siguiente_posición pos dire) (\pos -> pos) -}
posición_personaje = foldPersonaje (curry fst) siguiente_posición id

nombre_objeto :: Objeto -> String
{- nombre_objeto = foldObjeto (\pos nombre -> nombre) (\ob pj -> ob) (\ob -> ob) -}
nombre_objeto = foldObjeto (curry snd) (curry fst) id

{-Ejercicio 3-}

objetos_en :: [Either Personaje Objeto] -> [Objeto]
objetos_en = foldr (\x acc -> if es_un_objeto x then (objeto_de x) :acc else acc) []


personajes_en :: [Either Personaje Objeto] -> [Personaje]
personajes_en = foldr (\x acc -> if es_un_personaje x then (personaje_de x):acc else acc) []

{-Ejercicio 4-}

objetos_en_posesión_de :: String -> [Either Personaje Objeto] -> [Objeto]
objetos_en_posesión_de nombre universo = foldr (\x acc -> if ((en_posesión_de nombre x)) then x:acc else acc) [] (objetos_en universo)


{-Ejercicio 5-}

distanciaPersonajeObjeto :: Personaje  -> Objeto -> Float
distanciaPersonajeObjeto e1 e2 = norma2 (posición_personaje e1) (posición_objeto e2)

distancias_objetos_libres per = foldr (\x acc -> if es_un_objeto x && objeto_libre (objeto_de x)  then ((distanciaPersonajeObjeto per (objeto_de x)),objeto_de x) :acc else acc) []

--devuelve la tupla de (minima distancia, objeto)
objeto_libre_mas_cercano :: Personaje -> Universo -> Objeto
objeto_libre_mas_cercano per uni = snd (minimo(distancias_objetos_libres per uni))


minimo :: Ord a => [(a,b)] -> (a,b)
minimo = foldr1 (\acc x -> if fst x < fst acc then x else acc)
