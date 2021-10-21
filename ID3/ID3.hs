module ID3 where

import Tipos
import Utils
import AuxID3

-- Algoritmo ID3

id3 :: (Eq a) => (Eq b) => [Atributo a] -> [Ejemplo a b] -> Arbol a b
id3 [] ejemplos = Hoja (mascomun ejemplos)
id3 atributos ejemplos =
    let mejoratributo = mejorclasifica atributos ejemplos
        new_atributos = elimina mejoratributo atributos
    in
    if fst (homogeneo ejemplos)
    then Hoja (snd (homogeneo ejemplos))
    else Nodo mejoratributo (creahijo new_atributos (atributoevaluado ejemplos mejoratributo))

-- Crea árbol hijo dividiendo el conjunto de ejemplos según el valor del atributo que mejor clasifica.

creahijo :: (Eq a) => (Eq b) => [Atributo a] -> (a -> [Ejemplo a b]) -> a -> Arbol a b 
creahijo atributos mejorclasificasegunvalor valor =
         let ejemplos = mejorclasificasegunvalor valor
         in id3 atributos ejemplos