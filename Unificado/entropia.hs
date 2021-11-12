module Entropia where

import Tipos
import Utils
import Data.Maybe
import Data.Either

entropia :: [Ejemplo] -> Double
entropia [] = 0
entropia ejemplos =
    let valores = posiblesValores $ atributoObjetivo $ head ejemplos
        n = lengthDouble ejemplos
        f = ocurrencia (map (getL.valorObjetivo) ejemplos)
    in foldl (\ac x -> let p = (f x) / n in ac - p * (logBase (fromIntegral 2)  p) )
    0 valores