module EvaluaC45 where

import TiposC45
import C45
import AuxC45
import EjemplosLluviaC45
import EjemplosC45
import UtilsC45
import Data.Maybe

predice :: [(Atributo,ValorAtrib)] -> Arbol -> String
predice ejemplo (Hoja prediccion) = prediccion
predice ejemplo (Nodo (Left atributo) obtenerhijo) =
        let valoratributo = getL $ snd $ head $ filter (\x -> fst x == (Left atributo)) ejemplo
        in predice ejemplo (obtenerhijo valoratributo)
predice ejemplo (Nodo (Right atributo) obtenerhijo) =
        let v = getR $ snd $ head $ filter (\x -> fst x == (Right atributo)) ejemplo
            u = fromJust $ umbral atributo
        in if v <= u then predice ejemplo (obtenerhijo "<=")
        else predice ejemplo (obtenerhijo ">")

ejemploSexo = [aleft (altura,"alto"), aright (peso, 85.0)]
ejemploLluvia =  [aleft (outlook,"sunny"), aleft (humidity,"high"), aleft (windy,"false"), aright (temperature, 28.0)]


arbolC45lluvia = runc45 atributosLluvia ejemplosLluviaC45
arbolC45Sexo = runc45 atributos ejemplos
 
