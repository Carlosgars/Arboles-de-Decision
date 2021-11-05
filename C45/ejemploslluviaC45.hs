module EjemplosLluviaC45 where

import TiposC45
import Data.Maybe

aleft par = (Left $ fst par, Left $ snd par)
aright par = (Right $ fst par, Right $ snd par)

outlook = D { dnombre = "outlook", posiblesvalores=["sunny", "overcast", "rainy"] }
temperature = C { cnombre ="temperature", rango=(0,30), umbral=Nothing }
humidity = D { dnombre ="humidity", posiblesvalores=["high","normal"] }
windy = D { dnombre ="windy", posiblesvalores=["true","false"]}

rains = D { dnombre ="rains", posiblesvalores=["yes","no"]}

atributosLluvia = [Left outlook,Right temperature,Left humidity,Left windy]
clasificacionLluvia = Left rains

labels = ["no","no","yes","yes","yes","no","yes","no","yes","yes","yes","yes","yes","no"]

correctlabels = [ aleft (rains,x) | x <- labels ]

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
          
unlabeledLeft = map (map aleft) unlabeledD
unlabeledRight = map (map aright) unlabeledC

unlabeled = map ( \(x,y) -> x ++ y ) (zip unlabeledLeft unlabeledRight)

ejemplosLluviaC45 = zip unlabeled correctlabels