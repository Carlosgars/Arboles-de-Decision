module Evaluar where

import Test
import Tipos
import Ejemplos
import Utils
import Data.Maybe

predice :: [(Atributo,ValorAtrib)] -> Arbol -> ValorAtrib
predice ejemplo (Hoja prediccion) = prediccion
predice ejemplo (Nodo (Left atributo) obtenerhijo) =
        let valoratributo = getL $ snd $ head $ filter (\x -> fst x == (Left atributo)) ejemplo
        in predice ejemplo (obtenerhijo valoratributo)
predice ejemplo (Nodo (Right atributo) obtenerhijo) =
        let v = getR $ snd $ head $ filter (\x -> fst x == (Right atributo)) ejemplo
            u = fromJust $ umbral atributo
        in if v <= u then predice ejemplo (obtenerhijo "<=")
        else predice ejemplo (obtenerhijo ">")

ejemploSexoD = [aleft (altura,"alto"), aleft (peso, "pesado")]
ejemploSexoC45 = [aleft (altura,"alto"), aright (pesoC, 85.0)]
ejemploSexoC = [aright (alturaC,2.0), aright (pesoC, 85.0)]

ejemploLluviaD =  [aleft (outlook,"sunny"), aleft (humidity,"high"), aleft (windy,"false"), aleft (temperature, "hot")]
ejemploLluviaC45 =  [aleft (outlook,"sunny"), aleft (humidity,"high"), aleft (windy,"false"), aright (temperatureC, 28.0)]
ejemploLluviaC =  [aright (outlookC,0.0), aright (humidityC,0.0), aright (windyC,1.0), aright (temperatureC, 28.0)]


p1 = predice ejemploSexoD arbolC45sexoD
p2 = predice ejemploSexoC45 arbolC45sexoDC
p3 = predice ejemploSexoC arbolC45sexoC

p4 = predice ejemploLluviaC45 arbolC45lluviaDC 
p5 = predice ejemploLluviaD arbolC45lluviaD 
p6 = predice ejemploLluviaC arbolC45lluviaC

p7 = predice ejemploSexoC arbolCARTsexo_clas
p8 = predice ejemploSexoC arbolCARTsexo_reg

p9 = predice ejemploLluviaC arbolCARTLluvia_clas 
p10 = predice ejemploLluviaC arbolCARTLluvia_reg