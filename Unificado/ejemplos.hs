module Ejemplos where

import Tipos
import Utils
import DiscretizarContinuo
import GananciaNormalizada

---------------------
--- Ejemplos SEXO ---
---------------------

sexo = D "sexo" ["hombre","mujer"] :: Discreto
sexoC = C "sexo" (0,1) Nothing :: Continuo
altura = D "altura" ["bajo","medio","alto"] :: Discreto
alturaC = C "altura" (0,2) Nothing :: Continuo
alturaC2 = C "altura" (0,2) (Just 1.5) :: Continuo
peso = D "peso" ["ligero", "pesado"] :: Discreto
pesoC = C "peso"  (50,110) Nothing :: Continuo

atributosSexoD = [Left altura, Left peso] :: [Atributo]
atributosSexoDC = [Left altura, Right pesoC] :: [Atributo]
atributosSexoC = [Right alturaC, Right pesoC] :: [Atributo]


ejemploDC1 = ([aleft (altura,"alto"), aright (pesoC, 90.0)], aleft (sexo,"hombre")) :: Ejemplo
ejemploDC2 = ([aleft (altura,"medio"), aright (pesoC,80.0)], aleft (sexo,"hombre")) :: Ejemplo
ejemploDC3 = ([aleft (altura,"bajo"), aright (pesoC, 50.0)], aleft (sexo,"mujer")) :: Ejemplo
ejemploDC4 = ([aleft (altura,"medio"), aright (pesoC, 60.0)], aleft (sexo,"mujer")) :: Ejemplo
ejemploDC5 = ([aleft (altura,"bajo"), aright (pesoC, 70.0)], aleft (sexo,"mujer")) :: Ejemplo
ejemplosSexoDC = [ejemploDC1,ejemploDC2,ejemploDC3,ejemploDC4,ejemploDC5] :: [Ejemplo]

ejemploD1 = ([aleft (altura,"alto"), aleft (peso, "pesado")], aleft (sexo,"hombre")) :: Ejemplo
ejemploD2 = ([aleft (altura,"medio"), aleft (peso,"pesado")], aleft (sexo,"hombre")) :: Ejemplo
ejemploD3 = ([aleft (altura,"bajo"), aleft (peso,"ligero")], aleft (sexo,"mujer")) :: Ejemplo
ejemploD4 = ([aleft (altura,"medio"), aleft (peso,"ligero")], aleft (sexo,"mujer")) :: Ejemplo
ejemploD5 = ([aleft (altura,"bajo"), aleft (peso,"ligero")], aleft (sexo,"mujer")) :: Ejemplo
ejemplosSexoD = [ejemploD1,ejemploD2,ejemploD3,ejemploD4,ejemploD5] :: [Ejemplo]

ejemploC1 = ([aright (alturaC, 2.0), aright (pesoC, 90.0)], aleft (sexo,"hombre")) :: Ejemplo
ejemploC2 = ([aright (alturaC, 1.0), aright (pesoC, 80.0)], aleft (sexo,"hombre")) :: Ejemplo
ejemploC3 = ([aright (alturaC, 0.0), aright (pesoC, 50.0)], aleft (sexo,"mujer")) :: Ejemplo
ejemploC4 = ([aright (alturaC, 1.0), aright (pesoC, 60.0)], aleft (sexo,"mujer")) :: Ejemplo
ejemploC5 = ([aright (alturaC, 0.0), aright (pesoC, 70.0)], aleft (sexo,"mujer")) :: Ejemplo
ejemplosSexoC = [ejemploC1,ejemploC2,ejemploC3,ejemploC4,ejemploC5] :: [Ejemplo]

ejemploCART1 = ([aright (alturaC, 2.0), aright (pesoC, 90.0)], aright (sexoC, 0.0)) :: Ejemplo
ejemploCART2 = ([aright (alturaC, 1.0), aright (pesoC, 70.0)], aright (sexoC, 0.0)) :: Ejemplo
ejemploCART3 = ([aright (alturaC, 0.0), aright (pesoC, 50.0)], aright (sexoC, 1.0)) :: Ejemplo
ejemploCART4 = ([aright (alturaC, 1.0), aright (pesoC, 60.0)], aright (sexoC, 1.0)) :: Ejemplo
ejemplOCART5 = ([aright (alturaC, 0.0), aright (pesoC, 80.0)], aright (sexoC, 1.0)) :: Ejemplo
ejemplosSexoCART = [ejemploCART1,ejemploCART1,ejemploCART1,ejemploCART1,ejemploCART1] :: [Ejemplo]

-----------------------
--- Ejemplos LLUVIA ---
-----------------------

outlook = D "outlook" ["sunny", "overcast", "rainy"] 
outlookC = C "outlook" (0,2) Nothing

temperature = D "temperature" ["hot","mild", "cool"]
temperatureC = C "temperature" (0,30) Nothing

humidity = D "humidity" ["high","normal"]
humidityC = C "humidity" (0,1) Nothing

windy = D "windy" ["true","false"]
windyC = C "windy" (0,1) Nothing

atributosLluviaCART = [Right outlookC, Right temperatureC, Right humidityC, Right windyC]
atributosLluviaC45 = [Left outlook, Right temperatureC, Left humidity, Left windy]
atributosLluviaID3 = [Left outlook, Left temperature, Left humidity, Left windy]

unlabeledD = [
          [(outlook,"sunny"), (humidity,"high"),(windy,"false")],
          [(outlook,"sunny"), (humidity, "high"), (windy,"true")],
          [(outlook,"overcast"), (humidity, "high"), (windy, "false")],
          [(outlook,"rainy"), (humidity, "high"), (windy,"false")],
          [(outlook,"rainy"), (humidity, "normal"), (windy,"false")],
          [(outlook, "rainy"), (humidity, "normal"), (windy, "true")],
          [(outlook, "overcast"), (humidity, "normal"), (windy,"true")],
          [(outlook, "sunny"), (humidity, "high"), (windy, "false")],
          [(outlook, "sunny"), (humidity, "normal"), (windy, "false")],
          [(outlook, "rainy"), (humidity, "normal"), (windy, "false")],
          [(outlook, "sunny"), (humidity, "normal"), (windy, "true")],
          [(outlook, "overcast"), (humidity, "high"), (windy, "true")],
          [(outlook, "overcast"), (humidity, "normal"), (windy, "false")],
          [(outlook, "rainy"), (humidity, "high"), (windy,"true")]
          ]

unlabeledC = [
          [(outlookC, 0.0), (humidityC, 0.0), (windyC, 1.0)],
          [(outlookC, 0.0), (humidityC, 0.0), (windyC, 0.0)],
          [(outlookC, 1.0), (humidityC, 0.0), (windyC, 1.0)],
          [(outlookC, 2.0), (humidityC, 0.0), (windyC, 1.0)],
          [(outlookC, 2.0), (humidityC, 1.0), (windyC, 1.0)],
          [(outlookC, 2.0), (humidityC, 1.0), (windyC, 0.0)],
          [(outlookC, 1.0), (humidityC, 1.0), (windyC, 0.0)],
          [(outlookC, 0.0), (humidityC, 0.0), (windyC, 1.0)],
          [(outlookC, 0.0), (humidityC, 1.0), (windyC, 1.0)],
          [(outlookC, 2.0), (humidityC, 1.0), (windyC, 1.0)],
          [(outlookC, 0.0), (humidityC, 1.0), (windyC, 0.0)],
          [(outlookC, 1.0), (humidityC, 0.0), (windyC, 0.0)],
          [(outlookC, 1.0), (humidityC, 1.0), (windyC, 1.0)],
          [(outlookC, 2.0), (humidityC, 0.0), (windyC, 0.0)]
          ]

unlabeledtempC = [
          [(temperatureC, 28.0)],
          [(temperatureC, 26.0)],
          [(temperatureC, 27.0)],
          [(temperatureC, 18.0)],
          [(temperatureC, 4.0)],
          [(temperatureC, 7.0)],
          [(temperatureC, 9.0)],
          [(temperatureC, 17.0)],
          [(temperatureC, 10.0)],
          [(temperatureC, 21.0)],
          [(temperatureC, 22.0)],
          [(temperatureC, 20.0)],
          [(temperatureC, 29.0)],
          [(temperatureC, 19.0)]
          ]

unlabeledtempD = [
          [(temperature, "hot")],
          [(temperature, "hot")],
          [(temperature, "hot")],
          [(temperature, "mild")],
          [(temperature, "cool")],
          [(temperature, "cool")],
          [(temperature, "cool")],
          [(temperature, "mild")],
          [(temperature, "cool")],
          [(temperature, "mild")],
          [(temperature, "mild")],
          [(temperature, "mild")],
          [(temperature, "hot")],
          [(temperature, "mild")]
          ]
          
unlabeledLeft = map (map aleft) unlabeledD
unlabeledCtempRight = map (map aright) unlabeledtempC
unlabeledDtempLeft = map (map aleft) unlabeledtempD

unlabeledRight = map (map aright) unlabeledC

unlabeledC45 = map ( \(x,y) -> x ++ y ) (zip unlabeledLeft unlabeledCtempRight)
unlabeledID3 = map ( \(x,y) -> x ++ y ) (zip unlabeledLeft unlabeledDtempLeft)
unlabeledCART = map ( \(x,y) -> x ++ y ) (zip unlabeledRight unlabeledCtempRight)

rains = D "rains" ["yes","no"]
prob_rains = C "prob_rains" (0,100) Nothing

labelsD = ["no","no","no","yes","yes","no","yes","no","yes","yes","yes","yes","yes","no"]
labelsLeft = [ aleft (rains,x) | x <- labelsD ]

labelsC = [15.0,23.0,26.0,77.0,65.0,5.0,95.0,30.0,70.0,86.0,75.0,19.0,99.0,1.0]
labelsRight = [ aright (prob_rains,x)  | x <- labelsC ]

ejemplosLluviaC45 = zip unlabeledC45 labelsLeft
ejemplosLluviaID3 = zip unlabeledID3 labelsLeft
ejemplosLluviaCARTclasificacion = zip unlabeledCART labelsLeft
ejemplosLluviaCARTregresion = zip unlabeledCART labelsRight
