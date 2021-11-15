module EjemplosSexo where

import Tipos

-- Atributo objetivo

sexo = D "sexo" ["hombre","mujer"] :: Discreto

-- Atributos --

altura = D "altura" ["bajo","medio","alto"] :: Discreto
pesoC = C "peso"  (50,110) Nothing          :: Continuo

atributosSexo = [Left altura, Right pesoC] :: [Atributo]

-- Ejemplos de entrenamiento --

ejemplo1 = ([aleft (altura, "alto"),  aright (pesoC, 90.0)], aleft (sexo, "hombre")) :: Ejemplo
ejemplo2 = ([aleft (altura, "medio"), aright (pesoC, 70.0)], aleft (sexo, "hombre")) :: Ejemplo
ejemplo3 = ([aleft (altura, "bajo"),  aright (pesoC, 50.0)], aleft (sexo, "mujer"))  :: Ejemplo
ejemplo4 = ([aleft (altura, "medio"), aright (pesoC, 60.0)], aleft (sexo, "mujer"))  :: Ejemplo
ejemplo5 = ([aleft (altura, "bajo"),  aright (pesoC, 80.0)], aleft (sexo, "mujer"))  :: Ejemplo

ejemplosSexo = [ejemplo1,ejemplo2,ejemplo3,ejemplo4,ejemplo5] :: [Ejemplo]

