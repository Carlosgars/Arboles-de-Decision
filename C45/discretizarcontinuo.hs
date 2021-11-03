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
        let v = map getR (valores (Right atributo) ejemplos)
            us = rmdups $ posiblesParticiones atributo v
            gan_inic = ganInfoNormCUmbral atributo ejemplos (head us)
        in mejorumbralAux atributo ejemplos (tail us) (head us) gan_inic

mejorumbralAux:: Continuo -> [Ejemplo] -> [Double] -> Double -> Double -> Double
mejorumbralAux atributo ejemplos [] umbral gan = umbral
mejorumbralAux atributo ejemplos (u:us) umbral gan =
        let new_gan = ganInfoNormCUmbral atributo ejemplos umbral
        in if new_gan > gan
        then mejorumbralAux atributo ejemplos us u new_gan
        else mejorumbralAux atributo ejemplos us umbral gan


