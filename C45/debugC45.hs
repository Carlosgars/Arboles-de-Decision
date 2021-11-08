module DebugC45
where

import C45
import TiposC45
import AuxC45
import DiscretizarContinuo
import UtilsC45
import GananciaNormalizada
import EjemplosC45
import EjemplosLluviaC45

-------SEXO------
-- Primera iteracion
atribs =  [Left altura, Right peso]
discretizados1 = map (discretizar ejemplos) atribs
mejoratributo = mejorclasifica discretizados1 ejemplos
mejoratributo2 = mejorclasifica2 discretizados1 ejemplos
ejemplosalto = evaluar ejemplos (Left altura)  "alto"
ejemplosmedio = evaluar ejemplos (Left altura)  "medio"
ejemplosbajo = evaluar ejemplos (Left altura)  "bajo"

-- Segunda iteracion
atribs2 =  elimina mejoratributo2 discretizados1

-- rama "alto"
discretizados2alto = map (discretizar ejemplosalto) atribs2
mejoratributo2alto = mejorclasifica2 discretizados2alto ejemplosalto
ejemplosaltomenor = evaluar ejemplosalto (Right peso)  "<="
ejemplosaltomayor = evaluar ejemplosalto (Right peso)  ">"

-- rama "medio"
discretizados2medio = map (discretizar ejemplosmedio) atribs2
mejoratributo2medio = mejorclasifica2 discretizados2medio ejemplosmedio
ejemplosmediomenor = evaluar ejemplosmedio (Right peso)  "<="
ejemplosmediomayor = evaluar ejemplosmedio (Right peso)  ">"

-- rama "bajo"
discretizados2bajo = map (discretizar ejemplosbajo) atribs2
mejoratributo2bajo = mejorclasifica2 discretizados2bajo ejemplosbajo
ejemplosbajomenor = evaluar ejemplosbajo (Right peso)  "<="
ejemplosbajomayor = evaluar ejemplosbajo (Right peso)  ">"


------LLUVIA-----

-- Inic
s = ejemplosLluviaC45
a = atributosLluvia

-- Primera iteracion
discr1 = map (discretizar s) a
mejorat1 = mejorclasifica2 discr1 s
e0 = entropia s

s_sunny = evaluar s mejorat1  "sunny"
s_overcast = evaluar s mejorat1  "overcast"
s_rainy = evaluar s mejorat1  "rainy"

s_high = evaluar s mejorat1  "high"
s_normal = evaluar s mejorat1  "normal"

a2 = elimina mejorat1 discr1

discr2 = map (discretizar s_high) a2

mejorat2high = mejorclasifica2 discr2 s_high
s_high_sunny = evaluar s_high mejorat2high  "sunny"
s_high_overcast = evaluar s_high mejorat2high  "overcast"
s_high_rainy = evaluar s_high mejorat2high  "rainy"

mejorat2normal = mejorclasifica2 discr2 s_normal
s_normal_true = evaluar s_normal mejorat2normal  "true"
s_normal_false = evaluar s_normal mejorat2normal  "false"

e1 = map entropia [s_sunny,s_overcast,s_rainy]

-- Segunda iteracion


-- rama "sunny"
discrsunny = map (discretizar s_sunny) a2
mejor2sunny = mejorclasifica2 discrsunny s_sunny
--ejemplosaltomenor = evaluar ejemplosalto (Right peso)  "<="
--ejemplosaltomayor = evaluar ejemplosalto (Right peso)  ">"

-- rama "overcast"
eshomogeneo = homogeneo s_overcast
--discrovercast = map (discretizar ejemplosalto) atribs2
--mejoratributo2alto = mejorclasifica2 discrovercast ejemplosalto
--ejemplosaltomenor = evaluar ejemplosalto (Right peso)  "<="
--ejemplosaltomayor = evaluar ejemplosalto (Right peso)  ">"

-- rama "rainy"
discrrainy = map (discretizar s_rainy) a2
mejor2rainy = mejorclasifica2 discrrainy s_rainy
--ejemplosaltomenor = evaluar ejemplosalto (Right peso)  "<="
--ejemplosaltomayor = evaluar ejemplosalto (Right peso)  ">"

-- <Left outlook|
-- [<Left humidity|
-- ["no","yes"]>,"yes",<Left windy|
-- ["no","yes"]>]>


--- Rama High -> Overcast
at3 = [Right temperature,Left windy]
discr3 = map (discretizar s_high_overcast2) at3 --Temperature Just 13.5
s_high_overcast2 = evaluar s_high mejorat2high "overcast"
mejorclasifica_s_high_overcast2 = mejorclasifica2 discr3 s_high_overcast2
lista_ganancias = (ganancias discr3 s_high_overcast2) -- ganancia temperature = NaN

ganancia_temperatura = ganancianorm s_high_overcast2 (head discr3)

ramamenor = evaluar s_high_overcast2 (head discr3) "<="
ramamayor = evaluar s_high_overcast2 (head discr3) ">"

atribtemp = (head discr3)
ejemp = s_high_overcast2

s1 = [ x | x <- ejemp, (getR $ valorAtributo x atribtemp) <= 13.5 ]
s2 = [ x | x <- ejemp, (getR $ valorAtributo x atribtemp) > 13.5 ]