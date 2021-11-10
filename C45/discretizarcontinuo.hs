module DiscretizarContinuo where

import TiposC45
import Data.Either
import Data.List
import UtilsC45
import Data.Maybe
import GananciaNormalizada
import EjemplosC45

posiblesParticiones :: Continuo -> [Double] -> [Double]
posiblesParticiones atributo valores =
           let xs = [fst $ rango atributo] ++ valores ++ [snd $ rango atributo]
           in [ (x + y) / 2 | (x,y) <- zip xs (tail xs) ]

discretizar :: [Ejemplo] -> Atributo -> Atributo
discretizar ejemplos (Left atributo) = (Left atributo)
discretizar ejemplos (Right atributo) =
        let umbral = mejorumbral atributo ejemplos
        in Right( C (cnombre atributo) (rango atributo) (Just umbral))

mejorumbral :: Continuo -> [Ejemplo] -> Double
mejorumbral atributo ejemplos =
        let v = ordensindups $ map getR (valores (Right atributo) ejemplos)
            us = posiblesParticiones atributo v
            um_inic = head us
            gan_inic = ganInfoNormCUmbral atributo ejemplos um_inic
        in mejorumbralAux atributo ejemplos (tail us) um_inic gan_inic

mejorumbralAux:: Continuo -> [Ejemplo] -> [Double] -> Double -> Double -> Double
mejorumbralAux _ _ [] umbral _ = umbral
mejorumbralAux atributo ejemplos (u:us) umbral gan =
        let new_gan = ganInfoNormCUmbral atributo ejemplos umbral
        in if new_gan > gan
        then mejorumbralAux atributo ejemplos us u new_gan
        else mejorumbralAux atributo ejemplos us umbral gan


