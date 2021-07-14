# Arboles-de-Decision
Arboles de Decision - TFG

Título: Desarrollo de una librería Haskell sobre árboles de decisión
Alumno: Carlos García Sancho

 Descripción:
-------------------------------------------------------------------------------

Desarrollar una librería en el lenguaje de programación funcional Haskell que
permita la creación de árboles de decisión mediante algoritmos de aprendizaje.
La librería debe incluir la creación de un tipo abstracto de dato para
representar los árboles de decisión, algoritmos de aprendizaje de árboles de
decisión con distintas funciones para cuantificar el grado de dispersión, así
como su utilización para clasificar nuevas instancias.

 Objetivos:
-------------------------------------------------------------------------------

Objetivos de aprendizaje:
1- Aprender el modelo de predicción basado en árboles de decisión y sus
aplicaciones. 
2- Aprender a desarrollar librerías en el lenguaje de programación Haskell.
3- Aprender a utilizar datos mutables en el lenguaje de programación Haskell.
4- Aprender a utilizar el sistema de edición LaTeX para preparar documentación.

Objetivos de desarrollo:
1- Implementar en Haskell un tipo de dato/clase que de soporte a la creación de
árboles de decisión de distintos tipos, según los atributos sean discretos o
continuos. 
2- Implementar en Haskell funciones para cuantificar el grado de dispersión:
tasa de error, índice de Gini, grado de entropía, ganancia de información.
3- Implementar en Haskell funciones de aprendizaje de árboles de decisión a
partir de conjuntos de ejemplos, tanto para atributos discretos (algoritmo ID3)
como para atributos continuos (algoritmo CART).
4- Implementar en Haskell funcionalidades para clasificar nuevos ejemplos
usando un árbol de decisión.
5- Investigar la posibilidad de incorporar aprendizaje de grupos de árboles
(algoritmo Random Forest) y su utilización para la clasificación mayoritaria.
6- Realizar la documentación del trabajo en el sistema de edición LaTeX
utilizando la plataforma de edición colaborativa OverLeaf a partir de un
formato predefinido.

 Documentación:
-------------------------------------------------------------------------------
- Libro 'Machine Learning' de T. Mitchell (capítulos 3)
- Tema sobre árboles de decisión del Máster onLine de Informática.
- Documentación sobre árboles de decisión de Scikit-Learn
  (https://scikit-learn.org/stable/modules/tree.html). Aunque están
  implementadas en Python, las funcionalides incluidas en esta librería pueden
  servir de orientación para hacer las propuestas en el TfG
- Otras librerías sobre árboles de decisión implementadas en Haskell:
  DecisionTree (https://hackage.haskell.org/package/DecisionTree),
  HTrees (https://github.com/mreid/HTrees), ...
- Libro 'Programming in Haskell (2nd edition)' de G. Hutton
  (http://bit.ly/2pQofC2)

-------------------------------------------------------------------------------
