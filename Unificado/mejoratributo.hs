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
    foldl (\ac x -> let valor_new = f ejemplos x
                        valor_old = f ejemplos ac
           in if valor_new `operador` valor_old
              then x
              else ac) (head atributos) (tail atributos)

mejorClasificaGan = mejorClasifica gananciaNorm (>)
mejorClasificaECM = mejorClasifica ecmAtributo (<)
mejorClasificaGini = mejorClasifica giniAtributo (<)