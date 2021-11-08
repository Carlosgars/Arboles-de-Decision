module Gini where

import TiposCART
import UtilsCART
import EjemplosCART

gini :: [Ejemplo] -> Double
gini [] = 0
gini ejemplos = 
     let n = lengthDouble ejemplos
         clasificaciones = map (getL.clasificacion) ejemplos
         valores = posiblesvalores $ getL $ atributoObjetivo $ head ejemplos
         ocurr = map (\x -> fromIntegral $ ocurrencia clasificaciones x) valores
         prob = map (1/n*) ocurr
         probraised = map (^2) prob
     in 1 - sum probraised