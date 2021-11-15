module EjemplosLluvia where

import Tipos

---------------
-- Para C4.5 --
---------------

-- Atributo objetivo--

lluvia = D "lluvia" ["si","no"]

-- Atributos --

pronostico = D "pronostico" ["soleado", "nublado", "lluvioso"]

temperaturaC = C "temperatura" (0,30) Nothing

humedad = D "humedad" ["alta","normal","baja"]

viento = D "viento" ["no","si"]

atributos = [Left pronostico, Right temperaturaC, Left humedad, Left viento]

-- Ejemplos de entrenamiento --

unlabeledD = [
          [(pronostico, "soleado"),  (humedad, "alta"),   (windy, "no")],
          [(pronostico, "soleado"),  (humedad, "alta"),   (windy, "si")],
          [(pronostico, "nublado"),  (humedad, "normal"), (windy, "no")],
          [(pronostico, "lluvioso"), (humedad, "alta"),   (windy, "no")],
          [(pronostico, "lluvioso"), (humedad, "normal"), (windy, "no")],
          [(pronostico, "lluvioso"), (humedad, "normal"), (windy, "si")],
          [(pronostico, "nublado"),  (humedad, "normal"), (windy, "si")],
          [(pronostico, "soleado"),  (humedad, "alta"),   (windy, "no")],
          [(pronostico, "soleado"),  (humedad, "baja"),   (windy, "no")],
          [(pronostico, "lluvioso"), (humedad, "normal"), (windy, "no")],
          [(pronostico, "soleado"),  (humedad, "normal"), (windy, "si")],
          [(pronostico, "nublado"),  (humedad, "alta"),   (windy, "si")],
          [(pronostico, "nublado"),  (humedad, "baja"),   (windy, "no")],
          [(pronostico, "lluvioso"), (humedad, "alta"),   (windy, "si")]
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

unlabeledLeft = map (map aleft) unlabeledD
unlabeledtempRight = map (map aright) unlabeledtempC

unlabeled = map ( \(x,y) -> x ++ y ) (zip unlabeledLeft unlabeledCtempRight)

labels = ["no","no","no","yes","yes","no","yes","no","yes","yes","yes","yes","yes","no"]
labelsLeft = [ aleft (rains,x) | x <- labels ]

ejemplos = zip unlabeledC45 labelsLeft

-- Ejemplo a predecir --

ejemploLluviaC45 =  [aleft (pronostico,"soleado"), aleft (humedad,"alta"), aleft (viento,"no"), aright (temperaturaC, 28.0)]
