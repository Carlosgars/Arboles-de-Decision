module EvaluaArbol
where

import Tipos
import ID3
import AuxID3
import EjemplosSexo
import EjemplosLluvia
import Entropia

predice :: (Eq a) => [(Atributo a, a)] -> Arbol a b -> b
predice ejemplo (Hoja prediccion) = prediccion
predice ejemplo (Nodo atributo obtenerhijo) =
        let valoratributo = snd $ head $ filter (\x -> fst x == atributo) ejemplo
        in predice ejemplo (obtenerhijo valoratributo)


ejemploSexo = [(altura,"alto"),(peso,"pesado")]
ejemploLluvia =  [(outlook,"rainy"), (temperature,"mild"), (humidity,"normal"),(windy,"true")]


arbolID3lluvia = id3 atributosLluvia ejemplosLluvia
arbolID3Sexo = id3 atributosSexo ejemplosSexo