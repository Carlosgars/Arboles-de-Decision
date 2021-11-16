module ECM where

import TiposCART
import UtilsCART
import EjemplosCART


media :: [Double] -> Double
media xs = sum xs / lengthDouble xs

prediccionhoja :: [Ejemplo] -> Double
prediccionhoja ejemplos =
    let valoresobjetivo = map (getR.snd.snd) ejemplos
    in media $ map (getR.snd.snd) ejemplos
       
ecm :: [Ejemplo] -> Double
ecm ejemplos =
    let valoresobjetivo = map (getR.clasificacion) ejemplos
        prediccion = media valoresobjetivo
        ecms = map (\ x -> (x-prediccion)^2) valoresobjetivo
    in media ecms

ecm_atributo :: [Ejemplo] -> Atributo -> Double
ecm_atributo ejemplos atributo =
     let n = lengthDouble ejemplos
         s1 = evaluar ejemplos atributo "<="
         p1 = lengthDouble s1 / n
         s2 = evaluar ejemplos atributo ">"
         p2 = lengthDouble s2 / n
     in p1 * (ecm s1) + p2 * (ecm s2)

ecm_atributo_umbral :: [Ejemplo] -> Continuo -> Double -> Double
ecm_atributo_umbral ejemplos atributo umbral =
     let atrib = Right atributo
         n = lengthDouble ejemplos
         s1 = [ x | x <- ejemplos, (getR $ valorAtributo x atrib) <= umbral ]
         p1 = lengthDouble s1 / n
         s2 = [ x | x <- ejemplos, (getR $ valorAtributo x atrib) > umbral ]
         p2 = lengthDouble s2 / n
     in p1 * (ecm s1) + p2 * (ecm s2)