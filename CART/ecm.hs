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