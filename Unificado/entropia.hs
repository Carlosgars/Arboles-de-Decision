module Entropia where

import Tipos
import Utils
import Data.Maybe
import Data.Either

entropia :: [Ejemplo] -> Double
entropia [] = 0
entropia ejemplos =
        let valores = posiblesValores $ atributoObjetivo $ head ejemplos
        in sum $ map (calculoEntropia ejemplos) valores
          
calculoEntropia :: [Ejemplo] -> String -> Double
calculoEntropia ejemplos valor =
        let n = lengthDouble ejemplos
            frec = fromIntegral $ ocurrencia (map (getL.valorObjetivo) ejemplos) valor
            p = frec / n
        in
        if p > 0 then - p * (logBase (fromIntegral 2)  p)
        else 0