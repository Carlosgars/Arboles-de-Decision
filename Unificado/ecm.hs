module ECM where

import Tipos
import Utils


media :: [Double] -> Double
media xs = sum xs / lengthDouble xs

prediccionHoja :: [Ejemplo] -> Double
prediccionHoja ejemplos =
    let v = map (getR.valoresObjetivo) ejemplos
    in media v
       
ecm :: [Ejemplo] -> Double
ecm ejemplos =
    let v = map (getR.valoresObjetivo) ejemplos
        prediccion = media v
        ecms = map (\ x -> (x-prediccion)^2) v
    in media ecms

ecmAtributo :: [Ejemplo] -> Atributo -> Double
ecmAtributo ejemplos atributo =
     let n = lengthDouble ejemplos
         s1 = evaluar ejemplos atributo "<="
         p1 = lengthDouble s1 / n
         s2 = evaluar ejemplos atributo ">"
         p2 = lengthDouble s2 / n
     in p1 * (ecm s1) + p2 * (ecm s2)

ecmAtributoUmbral :: [Ejemplo] -> Continuo -> Double -> Double
ecmAtributoUmbral ejemplos atributo umbral =
     let atrib = Right atributo
         n = lengthDouble ejemplos
         s1 = [ x | x <- ejemplos, (getR $ valorAtributo x atrib) <= umbral ]
         p1 = lengthDouble s1 / n
         s2 = [ x | x <- ejemplos, (getR $ valorAtributo x atrib) > umbral ]
         p2 = lengthDouble s2 / n
     in p1 * (ecm s1) + p2 * (ecm s2)