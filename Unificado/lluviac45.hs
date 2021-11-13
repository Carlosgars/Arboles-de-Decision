module LluviaC45 where

import Tipos
import Utils

---------------
-- Para C4.5 --
---------------

-- Atributo objetivo--

lluvia = D "lluvia" ["si","no"]

-- Atributos --

pronostico = D "pronostico" ["soleado", "nublado", "lluvioso"]

temperaturaC = C "temperatura" (5,30) Nothing

humedad = D "humedad" ["alta","normal","baja"]

viento = D "viento" ["no","si"]

atributos = [Left pronostico, Right temperaturaC, Left humedad, Left viento]

-- Ejemplos de entrenamiento --

unlabeledD2 = [
          [(pronostico, "soleado"),  (humedad, "alta"),   (viento, "no")],
          [(pronostico, "soleado"),  (humedad, "alta"),   (viento, "si")],
          [(pronostico, "nublado"),  (humedad, "normal"), (viento, "no")],
          [(pronostico, "lluvioso"), (humedad, "alta"),   (viento, "no")],
          [(pronostico, "lluvioso"), (humedad, "normal"), (viento, "no")],
          [(pronostico, "lluvioso"), (humedad, "normal"), (viento, "si")],
          [(pronostico, "nublado"),  (humedad, "normal"), (viento, "si")],
          [(pronostico, "soleado"),  (humedad, "alta"),   (viento, "no")],
          [(pronostico, "soleado"),  (humedad, "baja"),   (viento, "no")],
          [(pronostico, "lluvioso"), (humedad, "normal"), (viento, "no")],
          [(pronostico, "soleado"),  (humedad, "normal"), (viento, "si")],
          [(pronostico, "nublado"),  (humedad, "alta"),   (viento, "si")],
          [(pronostico, "nublado"),  (humedad, "baja"),   (viento, "no")],
          [(pronostico, "lluvioso"), (humedad, "alta"),   (viento, "si")]
          ]

unlabeledD = [
          [(pronostico, "soleado"),  (humedad, "alta"),   (viento, "no")],
          [(pronostico, "soleado"),  (humedad, "alta"),   (viento, "si")],
          [(pronostico, "nublado"),  (humedad, "normal"), (viento, "no")],
          [(pronostico, "nublado"), (humedad, "alta"),   (viento, "no")],
          [(pronostico, "nublado"), (humedad, "normal"), (viento, "no")],
          [(pronostico, "nublado"), (humedad, "normal"), (viento, "si")],
          [(pronostico, "lluvioso"),  (humedad, "normal"), (viento, "si")],
          [(pronostico, "soleado"),  (humedad, "alta"),   (viento, "no")],
          [(pronostico, "soleado"),  (humedad, "baja"),   (viento, "no")],
          [(pronostico, "lluvioso"), (humedad, "normal"), (viento, "no")],
          [(pronostico, "soleado"),  (humedad, "normal"), (viento, "si")],
          [(pronostico, "lluvioso"),  (humedad, "alta"),   (viento, "si")],
          [(pronostico, "lluvioso"),  (humedad, "baja"),   (viento, "no")],
          [(pronostico, "lluvioso"), (humedad, "alta"),   (viento, "si")]
          ]

unlabeledtempC = [
          [(temperaturaC, 28.0)],
          [(temperaturaC, 26.0)],
          [(temperaturaC, 27.0)],
          [(temperaturaC, 18.0)],
          [(temperaturaC, 10.0)],
          [(temperaturaC, 8.0)],
          [(temperaturaC, 9.0)],
          [(temperaturaC, 17.0)],
          [(temperaturaC, 10.0)],
          [(temperaturaC, 21.0)],
          [(temperaturaC, 22.0)],
          [(temperaturaC, 20.0)],
          [(temperaturaC, 29.0)],
          [(temperaturaC, 19.0)]
          ]

unlabeledLeft = map (map aleft) unlabeledD
unlabeledtempRight = map (map aright) unlabeledtempC

unlabeled = map ( \(x,y) -> x ++ y ) (zip unlabeledLeft unlabeledtempRight)

labels = ["no","no","no","si","si","no","si","no","si","si","si","si","si","no"]
labelsLeft = [ aleft (lluvia, x) | x <- labels ]

ejemplos = zip unlabeled labelsLeft

-- Ejemplo a predecir --

ejemploLluviaC45 =  [aleft (pronostico,"soleado"), aleft (humedad,"alta"), aleft (viento,"no"), aright (temperaturaC, 28.0)]