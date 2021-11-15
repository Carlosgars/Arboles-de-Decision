module EjemploSimple
    where

import Tipos
import Utils
import ConstruirArbol
import Evaluar

-- Atributo Objetivo

color = D "COLOR" ["rojo","azul"]  :: Discreto

-- Atributos

x      = D  "X"  ["A","B"]           :: Discreto
y      = C  "Y"    (1,2)    Nothing  :: Continuo

atributos = [Left x, Right y]  :: [Atributo]

-- Ejemplos

ejemplo1 = ([aleft (x, "B"), aright (y, 2.0)], aleft (color, "azul")) :: Ejemplo
ejemplo2 = ([aleft (x, "A"), aright (y, 2.0)], aleft (color, "rojo")) :: Ejemplo
ejemplo3 = ([aleft (x, "A"), aright (y, 1.0)], aleft (color, "azul")) :: Ejemplo

ejemplos = [ejemplo1,ejemplo2,ejemplo3] :: [Ejemplo]

ejemploapredecir = [aleft (x,"B"), aright (y, 1.0)]  :: [(Atributo, ValorAtrib)]

-- Arbol

arbol = c45 1 atributos ejemplos  :: Arbol

-- Prediccion

prediccion = predice arbol ejemploapredecir  :: ValorAtrib
