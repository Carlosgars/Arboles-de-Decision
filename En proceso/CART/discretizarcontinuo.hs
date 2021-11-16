module DiscretizarContinuo where

import TiposCART
import Data.Either
import Data.List
import UtilsCART
import Data.Maybe
import Gini
import ECM
import EjemplosCART

posiblesParticiones :: Continuo -> [Double] -> [Double]
posiblesParticiones atributo valores =
           --let xs = [fst $ rango atributo] ++ valores ++ [snd $ rango atributo]
           let xs = valores
           in [ (x + y) / 2 | (x,y) <- zip xs (tail xs) ]

discretizar :: ([Ejemplo] -> Continuo -> Double -> Double) -> [Ejemplo] -> Atributo -> Atributo
discretizar _ _ (Left atributo) = (Left atributo)
discretizar f ejemplos (Right atributo) =
        let umbral = mejorumbral f atributo ejemplos
        in Right( C (cnombre atributo) (rango atributo) (Just umbral))

mejorumbral :: ([Ejemplo] -> Continuo -> Double -> Double) -> Continuo -> [Ejemplo] -> Double
mejorumbral f atributo ejemplos =
        let v = ordensindups $ map getR (valores (Right atributo) ejemplos)
            us =  posiblesParticiones atributo v
            um_inic = head us
            gan_inic = f ejemplos atributo um_inic 
        in mejorumbralAux f atributo ejemplos (tail us) um_inic gan_inic

mejorumbralAux:: ([Ejemplo] -> Continuo -> Double -> Double) -> Continuo -> [Ejemplo] -> [Double] -> Double -> Double -> Double
mejorumbralAux _ _ _ [] umbral _ = umbral
mejorumbralAux f atributo ejemplos (u:us) umbral valor_f =
        let new_valor_f = f ejemplos atributo umbral
        in if new_valor_f > valor_f
        then mejorumbralAux f atributo ejemplos us u new_valor_f
        else mejorumbralAux f atributo ejemplos us umbral valor_f

discretizarECM = discretizar ecm_atributo_umbral
discretizarGini = discretizar gini_atributo_umbral

