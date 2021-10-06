module Entropia where

import Tipos
import Utils

entropia :: (Eq b) => [Ejemplo a b] -> Double
entropia [] = 0
entropia ejemplos = entropiaaux ejemplos ((posiblesvalores.clasificacion.head) ejemplos) 0

entropiaaux :: (Eq b) => [Ejemplo a b] -> [b] -> Double -> Double
entropiaaux ejemplos [] ac = ac
entropiaaux ejemplos (c:posiblesvalores) ac =
            let p = (fromIntegral (ocurrencia c (map (snd.snd) ejemplos))) / (fromIntegral (length ejemplos))
            in
            if p > 0
            then entropiaaux ejemplos posiblesvalores (ac - p * (logBase (fromIntegral 2)  p))
            else entropiaaux ejemplos posiblesvalores ac