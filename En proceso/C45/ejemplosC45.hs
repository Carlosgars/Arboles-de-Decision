module EjemplosC45
where

import TiposC45

-- Ejemplos

aleft par = (Left $ fst par, Left $ snd par)
aright par = (Right $ fst par, Right $ snd par)

sexo = D "sexo" ["hombre","mujer"] :: Discreto
altura = D "altura" ["bajo","medio","alto"] :: Discreto
peso = C "peso"  (50,110) Nothing :: Continuo
nuevopeso = C "peso"  (50,110) (Just 55.0) :: Continuo

todosatributos = [Left sexo, Left altura, Right peso] :: [Atributo]
atributos =  [Left altura, Right peso] :: [Atributo]

ejemplo1 = ([aleft (altura,"alto"), aright (peso, 90.0)], aleft (sexo,"hombre")) :: Ejemplo
ejemplo2 = ([aleft (altura,"medio"), aright (peso,70)], aleft (sexo,"hombre")) :: Ejemplo
ejemplo3 = ([aleft (altura,"bajo"), aright (peso,50)], aleft (sexo,"mujer")) :: Ejemplo
ejemplo4 = ([aleft (altura,"medio"), aright (peso,60)], aleft (sexo,"mujer")) :: Ejemplo
ejemplo5 = ([aleft (altura,"bajo"), aright (peso,80)], aleft (sexo,"mujer")) :: Ejemplo
ejemplos = [ejemplo1,ejemplo2,ejemplo3,ejemplo4,ejemplo5] :: [Ejemplo]