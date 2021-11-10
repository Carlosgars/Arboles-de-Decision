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
         ocurr = map (fromIntegral.ocurrencia clasificaciones) valores
         prob = map (1/n*) ocurr
         probraised = map (^2) prob
     in 1 - sum probraised

gini_atributo :: [Ejemplo] -> Atributo -> Double
gini_atributo ejemplos atributo =
     let n = lengthDouble ejemplos
         s1 = evaluar ejemplos atributo "<="
         p1 = lengthDouble s1 / n
         s2 = evaluar ejemplos atributo ">"
         p2 = lengthDouble s2 / n
     in p1 * (gini s1) + p2 * (gini s2)

gini_atributo_umbral :: [Ejemplo] -> Continuo -> Double -> Double
gini_atributo_umbral ejemplos atributo umbral =
     let atrib = Right atributo
         n = lengthDouble ejemplos
         s1 = [ x | x <- ejemplos, (getR $ valorAtributo x atrib) <= umbral ]
         p1 = lengthDouble s1 / n
         s2 = [ x | x <- ejemplos, (getR $ valorAtributo x atrib) > umbral ]
         p2 = lengthDouble s2 / n
     in p1 * (gini s1) + p2 * (gini s2)