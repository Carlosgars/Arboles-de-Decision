module TiposDiversos
where

import Data.Either

data Discreto =
     D { dnombre :: String, posiblesvalores :: [String] }
        deriving Eq

data Continuo =
     C { cnombre :: String, rango :: [Double] }
        deriving Eq

instance Show Discreto where
  show = dnombre

instance Show Continuo where
  show = cnombre
  
type Atributo = Either Discreto Continuo

type ValorAtrib = Either String Double

type Ejemplo = ( [(Atributo, ValorAtrib)], (Atributo, ValorAtrib) )


aleft par = (Left $ fst par, Left $ snd par)
aright par = (Right $ fst par, Right $ snd par)

sexo = D "sexo" ["hombre","mujer"] :: Discreto
altura = D "altura" ["bajo","medio","alto"] :: Discreto
peso = C "peso"  [50,110] :: Continuo

atributos = [Left sexo, Left altura, Right peso] :: [Atributo]

getDiscreto = lefts
getContinuo = rights

ejemplo1 = ([aleft (altura,"alto"), aright (peso, 90.0)], aleft (sexo,"hombre")) :: Ejemplo
ejemplo2 = ([aleft (altura,"medio"), aright (peso,70)], aleft (sexo,"hombre")) :: Ejemplo
ejemplo3 = ([aleft (altura,"bajo"), aright (peso,50)], aleft (sexo,"mujer")) :: Ejemplo
ejemplo4 = ([aleft (altura,"medio"), aright (peso,60)], aleft (sexo,"mujer")) :: Ejemplo
ejemplo5 = ([aleft (altura,"bajo"), aright (peso,80)], aleft (sexo,"mujer")) :: Ejemplo
ejemplos = [ejemplo1,ejemplo2,ejemplo3,ejemplo4,ejemplo5] :: [Ejemplo]

-- data Arbol = Hoja ValorAtrib
--     | Nodo { atrib :: Atributo, hijo :: ValorAtrib -> Arbol }

funejemplo :: [Atributo] -> [String]
funejemplo [] = []
funejemplo ((Left x):xs) = "discreto": funejemplo xs
funejemplo ((Right x):xs) = "continuo": funejemplo xs

getL :: Either a b -> a
getL (Left x) = x


getR :: Either a b -> b
getR (Right x) = x

atributoObjetivo :: Ejemplo -> Atributo
atributoObjetivo = fst.snd

clasificacion :: Ejemplo -> ValorAtrib
clasificacion = snd.snd

