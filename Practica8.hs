--Tipos de Datos
data Nota = Maestria Char |Cato Int Int|Umss Int
    deriving Show

--1. Definir una función que reciba una nota y devuelva verdad(True) si 
--   esta es de la UMSS
notumss:: Nota -> Bool
notumss (Maestria _) = False
notumss (Cato _ _) = False
notumss (Umss _)= True


--2. Definir una función que reciba una nota y devuelva su valor sólo en
--   caso que sea de la Cato o de UMSS. En caso de ser de la Cato, que
--   devuelva el promedio de las dos notas.

data Valor = Ento Int | Ch Char deriving Show

valnota:: Nota -> Valor
valnota (Maestria x) = Ch x
valnota (Umss x) = Ento x
valnota (Cato x y) = Ento (div (x + y) 2) 

--3. Definir una función que reciba una lista de notas y devuelva 
--   únicamente las notas de aprobación. La nota de aprobación para el 
--   caso de la Cato es cuando el promedio de sus valores componentes 
--   es mayor a 50 y de la Maestría, cuando el valor es A,B o C.

aprob::Nota -> Bool
aprob (Maestria d) 
        |d == 'A' = True
        |d == 'B' = True
        |d == 'C' = True
        |otherwise = False
aprob (Umss x) = x > 50
aprob (Cato x y) = (div (x+y) 2 )>50

type Notas = [Nota]
aprobados::Notas -> Notas
aprobados [] = []
aprobados (x:xs) = if(aprob x) then x:aprobados xs else aprobados xs

nots = [(Cato 50 60), (Umss 70), (Umss 15), (Maestria 'B'), (Cato 15 70)] 

--4. Definir una función que reciba una lista de notas y las devuelva
--   ordenadas (suponer que una nota de San Simón es mayor a un nota de
--   la Cato y una de Maestría es mayor que una de la UMSS)
compnot:: Nota -> Nota -> Bool 
compnot (Umss x) (Maestria y) = False
compnot (Maestria y) (Umss x) = True 
compnot (Maestria x) (Cato n1 n2) = True 
compnot (Cato n1 n2) (Maestria x) = False 
compnot (Maestria x) (Maestria y) = True 
compnot (Umss x) (Umss y) = True 
compnot (Umss x) (Cato n1 n2) = True 
compnot (Cato n1 n2) (Umss x) = False 
compnot (Cato x1 x2) (Cato n1 n2) = True 


notord:: Notas -> Notas
notord [] = []
notord (x:xs) = aux xs x []
    where
        aux [] m r = m : notord r
        aux (x:xs) m r = if(compnot x m) then aux xs x (m:r) else aux xs m (x:r)


--5. Inventar 3 tipos de datos compuestos, para cada tipo inventar 3 
--   funciones que se apliquen al mismo


--6. Definir una función que reciba 4 número y devuelva la suma del mayor
--   de los 2 primeros con el mayor de los 2 siguientes utilizar la 
--   función mayor definida

data Rpta = Entero Int| Mensaje String
mayor x y 
            | x > y = Entero x
            | y > x = Entero y
            | otherwise = Mensaje "Iguales"



--7. Definir un tipo de datos para representar números enteros (Positivos 
--   y negativos).
data Enteros = Positivos Pos | Negativos Neg 
    deriving Show
type Pos = Int
type Neg = Int

--Utilizando este tipo definir los operadores: +,-,*,div
data Operadores a b = Suma a b | Resta a b | Division a b | Multiplicacion a b

--8. Definir una función que reciba dos listas xs, ys (del tipo Lista a) 
--   y devuelva cuantas veces ocurre xs en ys.


--9. Definir un tipo de datos para modelar árboles que se bifurcan en 
--   tres y guardan información únicamente en las hojas. Utilizando este
--   tipo definir funciones para:


--a) Calcular el total de hojas


--b) Calcular el total de nodos no terminales


--c) Calcular la sumatoria de las hojas.


--d) Comparar dos árboles.


--e) Comparar una lista de árboles y devolver verdad si todos son iguales.


--f) Definir la función foldTree


--g) Utilizando la función foldTree definir las funciones de los incisos
--   a, b, c.


--h) Añadir el tipo a la clase Eq


--10. Definir un tipo de datos para modelar árboles que se bifurcan en 
--    tres y guardan información en los nodos no terminales y en las 
--    hojas. Utilizando este tipo definir funciones para:

--a) Calcular el total de hojas
--b) Calcular el total de nodos no terminales
--c) Calcular la sumatoria de las hojas.
--d) Calcular la sumatoria de los valores de los nodos no terminales.
--e) Comparar dos árboles.
--f) Comparar una lista de árboles y devolver verdad si todos son iguales.
--g) Definir la función foldTree
--h) Utilizando la función foldTree definir las funciones de los incisos 
--   a, b, c, d.
--i) Añadir el tipo a la clase Eq

--11. Definir un tipo de datos para modelar árboles que se bifurcan en 
--    tres o en dos ramas y guardan información únicamente en las hojas.
--    Utilizando este tipo definir
--funciones para:
--a) Calcular el total de hojas
--b) Calcular el total de nodos no terminales
--c) Calcular la sumatoria de las hojas.
--d) Comparar dos árboles.
--e) Comparar una lista xs de árboles y devolver verdad si todos son 
--   iguales.
--i. Representar xs como [a]
--ii. Representar xs como Lista a
--f) Definir la función foldTree
--g) Utilizando la función foldTree definir las funciones de los 
--   incisos a, b, c, e.
--h) Añadir el tipo a la clase Eq