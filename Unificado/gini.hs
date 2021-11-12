module Gini where

import Tipos
import Utils

gini :: [Ejemplo] -> Double
gini [] = 0
gini ejemplos = 
     let n = lengthDouble ejemplos
         clasificaciones = map (getL.valorObjetivo) ejemplos
         valores = posiblesValores $ atributoObjetivo $ head ejemplos
         ocurr = map (ocurrencia clasificaciones) valores
         prob = map (1/n*) ocurr
         probraised = map (^2) prob
     in 1 - sum probraised

giniAtributo :: [Ejemplo] -> Atributo -> Double
giniAtributo ejemplos atributo =
     let n = lengthDouble ejemplos
         s1 = evaluar ejemplos atributo "<="
         p1 = lengthDouble s1 / n
         s2 = evaluar ejemplos atributo ">"
         p2 = lengthDouble s2 / n
     in p1 * (gini s1) + p2 * (gini s2)

giniAtributoUmbral :: [Ejemplo] -> Continuo -> Double -> Double
giniAtributoUmbral ejemplos atributo umbral =
     let atrib = Right atributo
         n = lengthDouble ejemplos
         s1 = [ x | x <- ejemplos, (getR $ valorAtributo x atrib) <= umbral ]
         p1 = lengthDouble s1 / n
         s2 = [ x | x <- ejemplos, (getR $ valorAtributo x atrib) > umbral ]
         p2 = lengthDouble s2 / n
     in p1 * (gini s1) + p2 * (gini s2)