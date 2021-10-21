type Ejemplo a b = ([(Atributo a,a)],(Atributo b,b))

data Atributo a =
     A { nombre :: String, tipo :: TipoAtr, rango :: [a] }
        deriving Eq
        
instance Show (Atributo a) where
  show = nombre
  

data TipoAtr = Discreto | Continuo
     deriving Eq

data Arbol a b = Hoja b
     | Nodo { atrib :: Atributo a, hijo :: a -> (Arbol a b) }



instance (Show a, Show b) => Show (Arbol a b) where
     show x = showTree x ""

showTree :: (Show a, Show b) => (Arbol a b) -> ShowS
showTree (Hoja x) = shows x
showTree (Nodo atrib hijo) = ('<':).shows atrib.("|\n"++).showList [hijo a | a <- rango atrib].('>':)


sexo = A "sexo" Discreto ["hombre","mujer"] :: Atributo String
altura = A "altura" Discreto ["bajo","medio","alto"] :: Atributo String
peso = A "peso" Continuo [0,150] :: Atributo Int

-- ejemplo1 = ([(altura,"alto"),(peso,100)],(sexo,"hombre")) :: Ejemplo String String
-- ejemplo2 = ([(altura,"medio"),(peso,"pesado")],(sexo,"hombre")) :: Ejemplo String String
-- ejemplo3 = ([(altura,"bajo"),(peso,"ligero")],(sexo,"mujer")) :: Ejemplo String String
-- ejemplo4 = ([(altura,"medio"),(peso,"ligero")],(sexo,"mujer")) :: Ejemplo String String
-- ejemplo5 = ([(altura,"bajo"),(peso,"pesado")],(sexo,"mujer")) :: Ejemplo String String

desconocido = ["alto","pesado"]

--ejemplosSexo = [ejemplo1,ejemplo2,ejemplo3,ejemplo4,ejemplo5] :: [Ejemplo String String]

