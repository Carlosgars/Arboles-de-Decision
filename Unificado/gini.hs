module Gini where

import Tipos
import Utils
import Data.Maybe

gini :: [Ejemplo] -> Double
gini [] = 0
gini ejemplos = 
    let n               = lengthDouble ejemplos
        clasificaciones = map (getL.valorObjetivo) ejemplos
        valores         = posiblesValores $ atributoObjetivo $ head ejemplos
    in 1 - foldl (\ac x -> let p = (ocurrencia clasificaciones x) / n
                   in ac + p^2) 0 valores

giniAtributo :: [Ejemplo] -> Atributo -> Double
giniAtributo ejemplos atributo =
    giniAtributoUmbral ejemplos (getR atributo) (fromJust $ umbral (getR $ atributo))
     
giniAtributoUmbral :: [Ejemplo] -> Continuo -> Double -> Double
giniAtributoUmbral ejemplos atributo umbral =
    let atrib = Right atributo
        n     = lengthDouble ejemplos
        s1    = [ x | x <- ejemplos, (getR $ valorAtributo x atrib) <= umbral ]
        p1    = lengthDouble s1 / n
        s2    = [ x | x <- ejemplos, (getR $ valorAtributo x atrib) > umbral ]
        p2    = lengthDouble s2 / n
    in p1 * (gini s1) + p2 * (gini s2)