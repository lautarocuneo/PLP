

{-- Tipos --}

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

{-- Observadores y funciones básicas de los tipos --}

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

objetos_libres_en :: Universo -> [Objeto]
objetos_libres_en u = filter objeto_libre (objetos_en u)

está_el_personaje :: String -> Universo -> Bool
está_el_personaje n = foldr (\x r -> es_un_personaje x && nombre x == n && (está_vivo $ personaje_de x) || r) False

está_el_objeto :: String -> Universo -> Bool
está_el_objeto n = foldr (\x r -> es_un_objeto x && nombre x == n && not (fue_destruido $ objeto_de x) || r) False

-- Asume que el personaje está
personaje_de_nombre :: String -> Universo -> Personaje
personaje_de_nombre n u = foldr1 (\x1 x2 -> if nombre_personaje x1 == n then x1 else x2) (personajes_en u)

-- Asume que el objeto está
objeto_de_nombre :: String -> Universo -> Objeto
objeto_de_nombre n u = foldr1 (\x1 x2 -> if nombre_objeto x1 == n then x1 else x2) (objetos_en u)

es_una_gema :: Objeto -> Bool
es_una_gema o = isPrefixOf "Gema de" (nombre_objeto o)

{- EJERCICIOS -}

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
posición_personaje = foldPersonaje (curry fst) siguiente_posición id

nombre_objeto :: Objeto -> String
nombre_objeto = foldObjeto (curry snd) (curry fst) id



{-Ejercicio 3-}

objetos_en :: Universo -> [Objeto]
objetos_en u = [objeto_de x | x <- u, es_un_objeto x ]

personajes_en :: Universo -> [Personaje]
personajes_en u = [personaje_de x | x <- u, es_un_personaje x]

{- DEMOSTRACIÓN ej3 -}

{-

  queremos demostrar:

    forall u :: Universo, forall o :: Objeto
      elem o (objetos_en u) -> elem (Right o) u

  es decir, que
    si el objeto se encuentra en la lista de objetos de u,
    entonces se encuentra también en formato Either en la lista universal

  pero podemos demostrar incluso algo más fuerte
    forall u :: Universo, forall o :: Objeto
      elem o (objetos_en u) == elem (Right o) u

  definición de "objetos_en"
    objetos_en u = [objeto_de x | x <- u, es_un_objeto x ]

  definición de "es_un_objeto"
    es_un_objeto (Left o) = False
    es_un_objeto (Right p) = True

  definición del tipo "Universo"
    type Universo = [Either Personaje Objeto]
    -- creo que deberíamos aplicar el principio de extensionalidad con forall p :: Personaje, forall o Objeto, tazu

  "elem e xs" devuelve "True" si el elemento "e" se encuentra en la lista "xs"
    creo que no importa la implementación de elem

-}

{-Ejercicio 4-}

objetos_en_posesión_de :: String -> Universo -> [Objeto]
objetos_en_posesión_de pj u = filter (en_posesión_de pj) (objetos_en u)



{-Ejercicio 5-}

--esta funcion le paso una persona como argumento, y me arma una lista de todos los objetos libres del universo, con sus distancias respecto al personaje (es una lista de tuplas (objetoLibre,DistanciaConRespectoApersonaje))
distancias_objetos_libres :: Personaje -> Universo -> [(Float , Objeto)]
distancias_objetos_libres pj u = [ (distancia (Left pj) (Right x), x) | x <- (objetos_en u) , objeto_libre x]

--devuelve el objeto mas cercano, que seria la tupla con la minima distancia de la lista que arme antes.
objeto_libre_mas_cercano :: Personaje -> Universo -> Objeto
objeto_libre_mas_cercano pj u = snd (minimo(distancias_objetos_libres pj u))

--calcula la tupla minima de una lista de tuplas, fijandose el primer valor (o sea las distancias.)
minimo :: Ord a => [(a,b)] -> (a,b)
minimo = foldr1 (\acc x -> if fst x < fst acc then x else acc)

{- 
DOS ALTERNATIVAS de la primera función, usando "objetos_libres_en", de TAZU

distancias_objetos_libres pj u = [ (distancia (Left pj) (Right x), x) | x <- (objetos_libres_en u) , True]

distancias_objetos_libres pj u = map (\x -> (distancia (Left pj) (Right x), x)) (objetos_libres_en u)
-}

{- 
ALTERNATIVA total, de TAZU

objeto_libre_mas_cercano :: Personaje -> Universo -> Objeto
objeto_libre_mas_cercano pj u = foldr1 (mas_cercano_aux pj) (objetos_libres_en u)

mas_cercano_aux :: Personaje -> [Objeto] -> Objeto
mas_cercano_aux x acc = if (distancia (Left pj) (Right x)) < (distancia (Left pj) (Right acc)) then x else acc -}



{-Ejercicio 6-}

las_Tiene :: [Objeto] -> Bool
las_Tiene lista = contarGemas lista == 6
  where
    -- cuenta cuántos elementos en la lista son gemas
    contarGemas :: [Objeto] -> Int
    contarGemas [] = 0
    contarGemas (x:xs)
      | es_una_gema x = 1 + contarGemas xs
      | otherwise = contarGemas xs


tiene_thanos_todas_las_gemas :: Universo -> Bool
tiene_thanos_todas_las_gemas u = if (está_el_personaje ("Thanos", u))
                                  then las_tiene (objetos_en_posesión_de ("Thanos", u))






{-Ejercicio 7-}

{-toma un universo y devuelve si en ese universo tenemos alguna chance de ganarle a thanos

  condiciones para ganarle:
  -thanos no haya conseguido todas las gemas
  -thor esta en el universo y tiene a stormbreaker
  -wanda y vision tienen que estar en el universo
  -vision tiene la gema de la mente.
-}
podemos_ganarle_a_thanos :: Universo -> Bool 
podemos_ganarle_a_thanos u = not(tiene_thanos_todas_las_gemas u) && (está_el_personaje "Thor" u) && (está_el_objeto "StormBreaker" u) 
&& (está "StormBreaker" (objetos_en_posesión_de "Thor" u)) && (está_el_personaje "Wanda" u) && (está_el_personaje "Visión" u) && (está_el_objeto "Gema de la Mente" u) 
&& (está "Gema de la Mente" (objetos_en_posesión_de "Visión" u))



está :: String -> [Objeto] -> Bool
está nombre = foldr (\x xs -> (nombre_objeto x) == nombre || xs) False









{-Tests-}

main :: IO Counts
main = do runTestTT allTests

allTests = test [ -- Reemplazar los tests de prueba por tests propios
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7
  ]




{- PERSONAJES -}

phil = Personaje (0,0) "Phil"
mjölnir = Objeto (2,2) "Mjölnir"

thanos = Personaje (0,0) "Thanos"

thor = Personaje (0,0) "Thor"
wanda = Personaje (0,0) "Wanda"
vision = Personaje (0,0) "Visión"
iron_man = Personaje (0,0) "Iron Man"
capitan_america = Personaje (0,0) "Capitán América"

{- OBJETOS -}

stormbreaker = Objeto (0,6) "StormBreaker"
escudo = Objeto (0,-1) "Escudo del Capitán América"

gema1 = Objeto (1,1) "Gema de la Realidad"
gema2 = Objeto (2,2) "Gema del Alma"
gema3 = Objeto (3,3) "Gema del Espacio"
gema4 = Objeto (4,4) "Gema del Poder"
gema5 = Objeto (5,5) "Gema del Tiempo"
gema6 = Objeto (6,6) "Gema de la Mente"

gemas = [gema1, gema2, gema3, gema4, gema5, gema6]

gema1_thanos = Tomado gema1 (thanos_con_n_gemas 1)
gema2_thanos = Tomado gema2 (thanos_con_n_gemas 2)
gema3_thanos = Tomado gema3 (thanos_con_n_gemas 3)
gema4_thanos = Tomado gema4 (thanos_con_n_gemas 4)
gema5_thanos = Tomado gema5 (thanos_con_n_gemas 5)
gema6_thanos = Tomado gema6 (thanos_con_n_gemas 6)

gemas_thanos = [gema1_thanos, gema2_thanos, gema3_thanos, gema4_thanos, gema5_thanos, gema6_thanos]

gemas_algunas_thanos = [gema1_thanos, gema2_thanos, gema3_thanos, gema4, gema5, gema6]

{- AUX -}

agarra_gemas :: Int -> Personaje -> Personaje
agarra_gemas n pj = if n > 0 then agarra_gemas (n-1) (Mueve (Mueve pj Norte) Este) else pj

thanos_con_n_gemas :: Int -> Personaje
thanos_con_n_gemas n = agarra_gemas n thanos

{- UNIVERSOS -}
-- entiendo que en cada universo deberían haber solo 6 gemas; podríamos preguntar

universo_sin_thanos = universo_con [phil] [mjölnir]

universo_solo_heroes_sin_objetos = universo_con [thor, wanda, vision, iron_man, capitan_america] []

universo_con_thanos_sin_gemas = universo_con [thanos] gemas
universo_con_thanos_algunas_gemas = universo_con [thanos_con_n_gemas 3] gemas_algunas_thanos
universo_con_thanos_todas_gemas = universo_con [thanos_con_n_gemas 6] gemas_thanos -- ya perdimos

universo_con_gemas = universo_con [] gemas

{- TESTS -}


testsEj1 = test [ -- Casos de test para el ejercicio 1
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) phil             -- Caso de test 1 - expresión a testear
    ~=? 0                                                               -- Caso de test 1 - resultado esperado
  ,
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) (Muere phil)     -- Caso de test 2 - expresión a testear
    ~=? 1                                                               -- Caso de test 2 - resultado esperado
  ]

testsEj2 = test [ -- Casos de test para el ejercicio 2
  posición_personaje phil       -- Caso de test 1 - expresión a testear
    ~=? (0,0)                   -- Caso de test 1 - resultado esperado
  ]

testsEj3 = test [ -- Casos de test para el ejercicio 3
  objetos_en []       -- Caso de test 1 - expresión a testear
    ~=? []            -- Caso de test 1 - resultado esperado
  ,
  objetos_en universo_con_gemas
    ~=? gemas
  ]

testsEj4 = test [ -- Casos de test para el ejercicio 4
  objetos_en_posesión_de "Phil" []       -- Caso de test 1 - expresión a testear
    ~=? []                             -- Caso de test 1 - resultado esperado
  ,
  objetos_en_posesión_de "Thanos" universo_con_thanos_todas_gemas
    ~=? gemas_thanos
  ]

testsEj5 = test [ -- Casos de test para el ejercicio 5
  objeto_libre_mas_cercano phil [Right mjölnir]       -- Caso de test 1 - expresión a testear
    ~=? mjölnir                                       -- Caso de test 1 - resultado esperado
  ]

testsEj6 = test [ -- Casos de test para el ejercicio 6
  tiene_thanos_todas_las_gemas universo_sin_thanos       -- Caso de test 1 - expresión a testear
    ~=? False                                            -- Caso de test 1 - resultado esperado
  ]

testsEj7 = test [ -- Casos de test para el ejercicio 7
  podemos_ganarle_a_thanos universo_sin_thanos         -- Caso de test 1 - expresión a testear
    ~=? False                                          -- Caso de test 1 - resultado esperado
  ]