module MejorAtributo where

import Tipos
import GananciaNormalizada
import DiscretizarContinuo
import Utils
import Gini
import ECM
import Data.Either

mejorClasifica :: ([Ejemplo] -> Atributo -> Double) -> (Double -> Double -> Bool) -> [Atributo] -> [Ejemplo] -> Atributo
mejorClasifica f operador atributos ejemplos =
    let at_inic = head atributos
        f_inic  = f ejemplos at_inic
    in fst $ foldl (\(at,f_at) x -> let f_x = f ejemplos x
                    in if f_x `operador` f_at
                    then (x, f_x)
                    else (at,f_at) )
             (head atributos, f_inic) (tail atributos)

mejorClasificaGan = mejorClasifica gananciaNorm (>)
mejorClasificaECM = mejorClasifica ecmAtributo (<)
mejorClasificaGini = mejorClasifica giniAtributo (<)