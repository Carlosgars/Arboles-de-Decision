module C45
where

import TiposC45
import AuxC45
import DiscretizarContinuo
import UtilsC45
import GananciaNormalizada
import EjemplosC45

c45 :: [Atributo] -> [Ejemplo] -> Arbol
c45 atributos ejemplos =
    let discretizados = map (discretizar ejemplos) atributos
        mejoratributo = mejorclasifica2 discretizados ejemplos
        new_atributos = elimina mejoratributo atributos
    in
    if fst (homogeneo ejemplos)
    then Hoja (snd (homogeneo ejemplos))
    else Nodo mejoratributo (creahijo new_atributos (evaluar ejemplos mejoratributo) )


creahijo :: [Atributo] -> (String -> [Ejemplo]) -> String -> Arbol
creahijo atributos mejorclasificasegunvalor valor =
         let ejemplos = mejorclasificasegunvalor valor
         in c45 atributos ejemplos