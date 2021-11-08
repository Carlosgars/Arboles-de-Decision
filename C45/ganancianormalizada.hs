module GananciaNormalizada where

import TiposC45
import Data.Either
import UtilsC45
import Data.Maybe
import EjemplosC45

--- Entropia ---
entropia :: [Ejemplo] -> Double
entropia [] = 0
entropia ejemplos =
          let valores = posiblesvalores $ getL $ atributoObjetivo $ head ejemplos
          in sum $ map (calculoentropia ejemplos) valores
          
calculoentropia :: [Ejemplo] -> String -> Double
calculoentropia ejemplos valor =
            let n = fromIntegral $ length ejemplos
                frec = ocurrencia valor (map (getL.snd.snd) ejemplos)
                p = fromIntegral (frec) / n
            in
            if p > 0 then - p * (logBase (fromIntegral 2)  p)
            else 0

--- Ganancia de Información ---

-- Discreto


ganancianormD ::[Ejemplo] -> Discreto -> Double
ganancianormD [] _ = 0
ganancianormD ejemplos atributo =
         let norm = normaD ejemplos atributo
             valores = posiblesvalores atributo
             ganancia = sum $ map (calculoGanD ejemplos atributo) valores
         in
         (entropia ejemplos - ganancia) / norm
         
calculoGanD :: [Ejemplo] -> Discreto -> String -> Double
calculoGanD [] atributo valor = 0
calculoGanD ejemplos atributo valor =
           let sv = evaluarDiscreto ejemplos valor atributo
           in
           (entropia sv) * (fromIntegral $ length sv) / (fromIntegral $ length ejemplos)

normaD :: [Ejemplo] -> Discreto -> Double
normaD [] _ = 1
normaD ejemplos atributo =
           let valores = posiblesvalores atributo
           in sum $ map (calculonorm ejemplos atributo) valores

calculonorm :: [Ejemplo] -> Discreto -> String -> Double
calculonorm [] atributo valor = 1
calculonorm ejemplos atributo valor =
        let n = fromIntegral $ length ejemplos
            vc = fromIntegral $ length $ evaluarDiscreto ejemplos valor atributo
            pc = vc / n
            output = - pc * (logBase (fromIntegral 2)  pc)
        in  - pc * (logBase (fromIntegral 2)  pc)
        

ganInfoNormCUmbral :: Continuo -> [Ejemplo] -> Double -> Double
ganInfoNormCUmbral _ [] _ = 0
ganInfoNormCUmbral atributo ejemplos umbral =
        -- let atrib = Right atributo
        --     s1 = [ x | x <- ejemplos, (getR $ valorAtributo x atrib) <= umbral ]
        --     s2 = [ x | x <- ejemplos, (getR $ valorAtributo x atrib) > umbral ]
        --     n = fromIntegral $ length ejemplos
        --     p1 = (fromIntegral $ length s1) / n
        --     p2 = (fromIntegral $ length s2) / n
        --     norm = - p1 * (logBase (fromIntegral 2)  p1) - p2 * (logBase (fromIntegral 2)  p2) 
        -- in
        -- if p1 /= 0 && p2 /= 0
        -- then (entropia ejemplos - (entropia s1) * p1 - (entropia s2) * p2) / norm
        -- else if p1 /= 0 && p2 == 0
        -- then (entropia ejemplos - (entropia s1) * p1) /  (- p1 * (logBase (fromIntegral 2)  p1))
        -- else if p1 == 0 && p2 /= 0
        -- then (entropia ejemplos - (entropia s2) * p2) /  (- p2 * (logBase (fromIntegral 2)  p2))
        -- else 0
        let atrib = Right atributo
            s1 = [ x | x <- ejemplos, (getR $ valorAtributo x atrib) <= umbral ]
            s2 = [ x | x <- ejemplos, (getR $ valorAtributo x atrib) > umbral ]
            n = fromIntegral $ length ejemplos
            norm = (calculonormC s1 n) + (calculonormC s2 n)
        in if norm/= 0 then (entropia ejemplos - (calculoganC s1 n) - (calculoganC s2 n)) / norm
        else 0 

calculoganC :: [Ejemplo] -> Double -> Double
calculoganC _ 0 = 0
calculoganC [] _ = 0
calculoganC s n =
        let p = (fromIntegral $ length s) / n
        in (entropia s) * p

calculonormC :: [Ejemplo] -> Double -> Double
calculonormC _ 0 = 0
calculonormC [] _ = 0
calculonormC s n =
        let p = (fromIntegral $ length s) / n
        in - p * (logBase (fromIntegral 2)  p) 

ganancianormC ::  [Ejemplo] -> Continuo -> Double
ganancianormC [] _ = 0
ganancianormC ejemplos atributo =
          let u = fromJust $ umbral atributo
          in ganInfoNormCUmbral atributo ejemplos u

ganancianorm :: [Ejemplo] -> Atributo -> Double
ganancianorm [] _ = 0
ganancianorm ejemplos atributo = either (ganancianormD ejemplos) (ganancianormC ejemplos) atributo

