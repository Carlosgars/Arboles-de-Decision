module Test where

import Ejemplos
import ConstruirModelos

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