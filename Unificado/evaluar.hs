module Evaluar where

import Tipos
import Utils
import Data.Maybe

predice ::  Arbol -> [(Atributo,ValorAtrib)] -> ValorAtrib
predice (Hoja prediccion) ejemplo = prediccion
predice (Nodo (Left atributo) obtenerhijo) ejemplo =
        let valoratributo = getL $ snd $ head $ filter (\x -> fst x == (Left atributo)) ejemplo
        in predice (obtenerhijo valoratributo) ejemplo
predice (Nodo (Right atributo) obtenerhijo) ejemplo =
        let v = getR $ snd $ head $ filter (\x -> fst x == (Right atributo)) ejemplo
            u = fromJust $ umbral atributo
        in if v <= u then predice (obtenerhijo "<=") ejemplo
        else predice (obtenerhijo ">") ejemplo

