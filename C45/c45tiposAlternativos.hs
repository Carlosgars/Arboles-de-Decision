
type Ejemplo a b =
     (
     [(Atributo a,a)],
     (Atributo b,b)
     )

data Discreto a =
     D { anombre :: String, posiblesvalores :: [a] }
        deriving Eq

data Continuo a =
     C { cnombre :: String, rango :: [a] }
        deriving Eq
        
instance Show (Discreto a) where
  show = anombre

instance Show (Continuo a) where
  show = cnombre

data Atributo a = Discreto a | Continuo a
     deriving Eq

data Arbol a b = Hoja b
     | Nodo { atrib :: Atributo a, hijo :: a -> (Arbol a b) }

sexo = D "sexo" ["hombre","mujer"] :: Discreto String
altura = D "altura" ["bajo","medio","alto"] :: Discreto String
peso = C "peso"  [0,150] :: Continuo Int

getnombre :: Atributo a -> String
getnombre = anombre