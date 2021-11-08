module EjemplosCART where

import TiposCART
import Data.Maybe

aleft par = (Left $ fst par, Left $ snd par)
aright par = (Right $ fst par, Right $ snd par)

outlook = D { dnombre = "outlook", posiblesvalores=["sunny", "overcast", "rainy"] }
outlookC = C { cnombre = "outlook", rango=(0,2), umbral=Nothing }
temperature = C { cnombre ="temperature", rango=(0,30), umbral=Nothing }
humidity = D { dnombre ="humidity", posiblesvalores=["high","normal"] }
humidityC = C { cnombre ="humidity", rango=(0,1), umbral=Nothing}
windy = D { dnombre ="windy", posiblesvalores=["true","false"]}
windyC = C { cnombre ="windy", rango=(0,1), umbral=Nothing}

rains = D { dnombre ="rains", posiblesvalores=["yes","no"]}
prob_rains = C { cnombre ="prob_rains", rango=(0,100), umbral=Nothing}

atLluviaCART = [Right outlookC, Right temperature, Left humidityC, Left windyC]
clasificacionLluvia = Right prob_rains

labels = [15.0,23.0,26.0,77.0,65.0,5.0,95.0,30.0,70.0,86.0,75.0,19.0,99.0,1.0] :: [Double]

correctlabels = [ (clasificacionLluvia,Right x) | x <- labels ] :: [(Either Discreto Continuo, Either String Double)]

unlabeledD = [
          [(outlookC, 0.0), (humidityC, 0.0),(windyC, 1.0)],
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
          
unlabeledC = [
          [(temperature, 28.0)],
          [(temperature, 26.0)],
          [(temperature, 27.0)],
          [(temperature, 18.0)],
          [(temperature, 4.0)],
          [(temperature, 7.0)],
          [(temperature, 9.0)],
          [(temperature, 17.0)],
          [(temperature, 10.0)],
          [(temperature, 21.0)],
          [(temperature, 22.0)],
          [(temperature, 20.0)],
          [(temperature, 29.0)],
          [(temperature,19.0)]
          ]
          
unlabeledLeft = map (map aright) unlabeledD
unlabeledRight = map (map aright) unlabeledC

unlabeled = map ( \(x,y) -> x ++ y ) (zip unlabeledLeft unlabeledRight)

ejemplosLluvia = zip unlabeled correctlabels :: [Ejemplo]