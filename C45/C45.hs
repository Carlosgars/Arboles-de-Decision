module C45
where

import TiposC45
import AuxC45
import DiscretizarContinuo
import UtilsC45
import GananciaNormalizada
import EjemplosC45
import EjemplosLluviaC45

runc45 = c45 "error: Ejemplos vacio."

c45 :: String -> [Atributo] -> [Ejemplo] -> Arbol
c45 comunpadre _ [] = Hoja comunpadre
c45 _ [] ejemplos = Hoja (mascomun ejemplos)
c45 comunpadre atributos ejemplos =
    let discretizados = map (discretizar ejemplos) atributos
        mejoratributo = mejorclasifica discretizados ejemplos
        new_atributos = elimina mejoratributo atributos
        posiblehoja = parada 0.75 ejemplos
        comunpadre = mascomun ejemplos
    in
    if fst posiblehoja
    then Hoja (snd posiblehoja)
    else Nodo mejoratributo (creahijo comunpadre new_atributos (evaluar ejemplos mejoratributo))


creahijo :: String -> [Atributo] -> (String -> [Ejemplo]) -> String -> Arbol
creahijo comunpadre atributos mejorclasificasegunvalor valor =
         let ejemplos = mejorclasificasegunvalor valor
         in c45 comunpadre atributos ejemplos


ranc45 = runc45 atributosLluvia ejemplosLluviaC45