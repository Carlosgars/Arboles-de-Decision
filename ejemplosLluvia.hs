module EjemplosLluvia where

import Tipos

outlook = A { nombre = "outlook", posiblesvalores=["sunny", "overcast", "rainy"] }
temperature = A { nombre ="temperature", posiblesvalores=["hot","mild", "cool"] }
humidity = A { nombre ="humidity", posiblesvalores=["high","normal"] }
windy = A { nombre ="windy", posiblesvalores=["true","false"]}

rains = A { nombre ="rains", posiblesvalores=["yes","no"]}

atributosLluvia = [outlook,temperature,humidity,windy]
clasificacionLluvia = rains

labels = ["no","no","yes","yes","yes","no","yes","no","yes","yes","yes","yes","yes","no"]

correctlabels = [ (rains,x) | x <- labels ]

ejemplosLluvia = zip unlabeled correctlabels

unlabeled = [
          [(outlook,"sunny"), (temperature,"hot"), (humidity,"high"),(windy,"false")],
          [(outlook,"sunny"), (temperature,"hot"), (humidity, "high"), (windy,"true")],
          [(outlook,"overcast"), (temperature,"hot"), (humidity, "high"), (windy, "false")],
          [(outlook,"rainy"), (temperature,"mild"), (humidity, "high"), (windy,"false")],
          [(outlook,"rainy"), (temperature, "cool"), (humidity, "normal"), (windy,"false")],
          [(outlook, "rainy"), (temperature,"cool"), (humidity, "normal"), (windy, "true")],
          [(outlook, "overcast"), (temperature, "cool"), (humidity, "normal"), (windy,"true")],
          [(outlook, "sunny"), (temperature, "mild"), (humidity, "high"), (windy, "false")],
          [(outlook, "sunny"), (temperature, "cool"), (humidity, "normal"), (windy, "false")],
          [(outlook, "rainy"), (temperature, "mild"), (humidity, "normal"), (windy, "false")],
          [(outlook, "sunny"), (temperature, "mild"), (humidity, "normal"), (windy, "true")],
          [(outlook, "overcast"), (temperature, "mild"), (humidity, "high"), (windy, "true")],
          [(outlook, "overcast"), (temperature, "hot"), (humidity, "normal"), (windy, "false")],
          [(outlook, "rainy"), (temperature, "mild"), (humidity, "high"), (windy,"true")]
          ]