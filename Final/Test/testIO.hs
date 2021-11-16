module TestIO
where

import System.IO
import Tipos
import Ejemplos
import System.IO.Unsafe

lista :: IO [Int]
lista = return [1,2,3,4]

wrapEjemplos :: [Ejemplo] -> IO [Ejemplo]
wrapEjemplos = return

pordoslista lista = map (*2) (unsafePerformIO lista)

main :: [Int]
main =
     do
     list <- unsafePerformIO lista
     return $ list