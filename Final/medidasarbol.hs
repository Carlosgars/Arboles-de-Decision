module MedidasArbol where

import Tipos
import Test
import Ejemplos
import Utils

profundidad :: Arbol -> Int
profundidad (Hoja x) = 1
profundidad (Nodo atrib hijo) =
    let valores = posiblesValores atrib
    in 1 + maximum ((map profundidad)  (map hijo valores))

numeroHojas :: Arbol -> Int
numeroHojas (Hoja x) = 1
numeroHojas (Nodo atrib hijo) =
    let valores = posiblesValores atrib
    in sum ((map numeroHojas)  (map hijo valores))