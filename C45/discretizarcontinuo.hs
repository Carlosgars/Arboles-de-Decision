module DiscretizarContinuo where

import TiposDiversos
import Data.Either
import Data.List

posiblesParticiones :: Continuo -> [Double] -> [Double]
posiblesParticiones atributo valores =
                    let xs = [head $ rango atributo] ++ valores ++ [last $ rango atributo]
                    in [ (x + y) / 2 | (x,y) <- zip xs (tail xs) ]

valores :: Atributo -> [Ejemplo] -> [ValorAtrib]
valores _ [] = []
valores atributo (e:ejemplos) = [ snd x | x <- fst e, fst x == atributo ]
        ++ valores atributo ejemplos

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

discretizarAtributo :: Continuo -> Double -> Discreto
discretizarAtributo atributo umbral =
                    let u = show umbral
                    in D ((cnombre atributo) ++ " discreto") ["<=" ++ u, ">"++ u]

posiblesValoresUmbral :: Continuo -> Double -> [String]
posiblesValoresUmbral atributo umbral =
                      posiblesvalores $ discretizarAtributo atributo umbral

evaluarContinuo ::  [Ejemplo] -> String -> Continuo -> [Ejemplo]
evaluarContinuo ejemplos valor atributo =
                    let umbral1 = read (tail $ tail valor) :: Double
                        umbral2 = read (tail valor) :: Double
                        atrib = Right atributo
                    in if head valor == '<'
                    then [ x | x <- ejemplos,
                    (getR $ valorAtributo x atrib) <= umbral1 ]
                    else [ x | x <- ejemplos,
                    (getR $ valorAtributo x atrib) > umbral2 ]
        

evaluarDiscreto :: [Ejemplo] -> String -> Discreto -> [Ejemplo]
evaluarDiscreto ejemplos valor atributo =
                let atrib = Left atributo 
                in [x | x <- ejemplos, (getL $ valorAtributo x atrib) == valor ]

-- either :: (Discreto -> [Ejemplo]) -> (Continuo -> [Ejemplo]) -> Atributo -> [Ejemplo]

evaluar :: [Ejemplo] -> String -> Atributo -> [Ejemplo]
evaluar ejemplos valor atributo =
         either (evaluarDiscreto ejemplos valor)
         (evaluarContinuo ejemplos valor) atributo


valorAtributo :: Ejemplo -> Atributo -> ValorAtrib
valorAtributo ejemplo atributo =
             snd $ head (filter (\x -> fst x == atributo) (fst ejemplo))
              

