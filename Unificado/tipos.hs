module Tipos where

import Data.Either
import Data.Maybe


-------------------
-- Tipo Discreto --
-------------------

data Discreto =
     D { nombreD :: String, posiblesvaloresD :: [String] }
        deriving Eq

instance Show Discreto where
  show = nombreD


-------------------
-- Tipo Continuo --
-------------------

data Continuo =
     C { nombreC :: String, rango :: (Double,Double), umbral :: Maybe Double }

instance Eq Continuo where
     continuo == continuo' =
              nombreC continuo == nombreC continuo'
              && rango continuo == rango continuo'
     
instance Show Continuo where
  show = nombreC


-------------------
-- Tipo Atributo --
-------------------

type Atributo = Either Discreto Continuo
type ValorAtrib = Either String Double


-----------------
--Tipo Ejemplo --
-----------------

type Ejemplo = ( [(Atributo, ValorAtrib)], (Atributo, ValorAtrib) )


----------------
-- Tipo Arbol --
----------------

data Arbol = Hoja ValorAtrib
     | Nodo { atrib::Atributo, hijo::String -> Arbol }


-----------------
-- Utils Tipos --
-----------------

instance Show Arbol where
     show x = showTree x ""
     
showTree :: Arbol -> ShowS
showTree (Hoja x) = shows x
showTree (Nodo atrib hijo) = ('<':).shows atrib.(showUmbral atrib ++).("|\n"++).showList [(hijo a,a) | a <- posiblesValores atrib].('>':)

nombre :: Atributo -> String
nombre (Left atributo) = nombreD atributo
nombre (Right atributo) = nombreC atributo

showUmbral :: Atributo -> String
showUmbral (Left atrib) = ""
showUmbral (Right atrib) =
    let u = umbral atrib
    in if isNothing u then ""
    else " {Umbral: " ++ (show $ fromJust $ u) ++ "}"

posiblesValores :: Atributo -> [String]
posiblesValores (Left atributo) = posiblesvaloresD atributo
posiblesValores (Right atributo) = ["<=",">"]