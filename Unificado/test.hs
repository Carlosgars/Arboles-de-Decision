module Test where

import Ejemplos
import ConstruirModelos
import Error
import Evaluar
import Utils

-- Crear arboles --
arbolC45sexoD = c45 atributosSexoD ejemplosSexoD
arbolC45sexoDC = c45 atributosSexoDC ejemplosSexoDC
arbolC45sexoC = c45 atributosSexoC ejemplosSexoC

arbolC45lluviaDC = c45 atributosLluviaC45 ejemplosLluviaC45
arbolC45lluviaD = c45 atributosLluviaID3 ejemplosLluviaID3
arbolC45lluviaC = c45 atributosLluviaCART ejemplosLluviaCARTclasificacion

arbolCARTsexo_clas = cart "clasificacion" atributosSexoC ejemplosSexoC
arbolCARTsexo_reg = cart "regresion" atributosSexoC ejemplosSexoCART

arbolCARTLluvia_clas = cart "clasificacion" atributosLluviaCART ejemplosLluviaCARTclasificacion
arbolCARTLluvia_reg = cart "regresion" atributosLluviaCART ejemplosLluviaCARTregresion

-- Ejemplos a predecir --

ejemploSexoD = [aleft (altura,"alto"), aleft (peso, "pesado")]
ejemploSexoC45 = [aleft (altura,"alto"), aright (pesoC, 85.0)]
ejemploSexoC = [aright (alturaC,2.0), aright (pesoC, 85.0)]

ejemploLluviaD =  [aleft (outlook,"sunny"), aleft (humidity,"high"), aleft (windy,"false"), aleft (temperature, "hot")]
ejemploLluviaC45 =  [aleft (outlook,"sunny"), aleft (humidity,"high"), aleft (windy,"false"), aright (temperatureC, 28.0)]
ejemploLluviaC =  [aright (outlookC,0.0), aright (humidityC,0.0), aright (windyC,1.0), aright (temperatureC, 28.0)]

-- Predicciones --
p1 = predice arbolC45sexoD ejemploSexoD
p2 = predice arbolC45sexoDC ejemploSexoC45
p3 = predice arbolC45sexoC ejemploSexoC
p4 = predice arbolC45lluviaDC ejemploLluviaC45
p5 = predice arbolC45lluviaD ejemploLluviaD
p6 = predice arbolC45lluviaC ejemploLluviaC
p7 = predice arbolCARTsexo_clas ejemploSexoC
p8 = predice arbolCARTsexo_reg ejemploSexoC
p9 = predice arbolCARTLluvia_clas ejemploLluviaC
p10 = predice arbolCARTLluvia_reg ejemploLluviaC

-- Errores en conjuntos de entrenamiento --
e1 = errorClasificacionConjunto arbolC45sexoD ejemplosSexoD
e2 = errorClasificacionConjunto arbolC45sexoDC ejemplosSexoDC
e3 = errorClasificacionConjunto arbolC45sexoC ejemplosSexoC

e4 = errorClasificacionConjunto arbolC45lluviaDC ejemplosLluviaC45
e5 = errorClasificacionConjunto arbolC45lluviaD ejemplosLluviaID3 
e6 = errorClasificacionConjunto arbolC45lluviaC ejemplosLluviaCARTclasificacion

e7 = errorClasificacionConjunto arbolCARTsexo_clas ejemplosSexoC
e8 = errorRegresionConjunto arbolCARTsexo_reg ejemplosSexoCART

e9 = errorClasificacionConjunto arbolCARTLluvia_clas ejemplosLluviaCARTclasificacion
e10 = errorRegresionConjunto arbolCARTLluvia_reg ejemplosLluviaCARTregresion