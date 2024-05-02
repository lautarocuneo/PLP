import Data.Char (isDigit)

-- Función para leer una lista de números de la entrada estándar
readNumbers :: IO [Int]
readNumbers = do
    putStrLn "Ingrese una lista de números separados por espacios:"
    input <- getLine
    let numbers = map read (words input)
    if all isDigit (unwords (words input))
        then return numbers
        else do
            putStrLn "Error: Ingrese solo números."
            readNumbers

-- Función principal (main)
main :: IO ()
main = do
    numbers <- readNumbers
    let absNumbers = map abs numbers
    putStrLn "La lista de números absolutos es:"
    print absNumbers