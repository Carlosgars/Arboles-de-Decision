module Evaluar where

import Tipos
import Utils
import Data.Maybe

predice ::  Arbol -> [(Atributo,ValorAtrib)] -> ValorAtrib
predice (Hoja prediccion) atrib_val = prediccion
predice (Nodo (Left atributo) hijo) atrib_val =
    let v = getL $ snd $ head $ filter (\x -> fst x == (Left atributo)) atrib_val
    in predice (hijo v) atrib_val
predice (Nodo (Right atributo) hijo) atrib_val =
    let v = getR $ snd $ head $ filter (\x -> fst x == (Right atributo)) atrib_val
        u = fromJust $ umbral atributo
    in if v <= u
       then predice (hijo "<=") atrib_val
       else predice (hijo ">") atrib_val

