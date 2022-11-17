--PRACTICA LISTAS POR COMPRENSIÓN

--I. Aplicando Listas por Comprensión, definir:
--1. filter
myfilter d xs= [x | x <- xs, (d)x]

--2. map
mymap f xs = [f x | x <- xs]

--3. concat
myconcat xss = [x | xs<-xss, x <- xs]

--4. length
mylength xs = sum [1 | _ <- xs]

--5. Una función que reciba una cadena y la encripte, cambiando las 
--vocales por los caracteres correspondientes a los dígitos 1,2,3,4,5 
--respectivamente.
encrip cs = [f c | c <- cs]
    where 
        f d |d == 'a' = '1'
            |d == 'e' = '2'
            |d == 'i' = '3'
            |d == 'o' = '4'
            |d == 'u' = '5'
            |otherwise = d

--6. Una función que realice el producto cartesiano de dos conjuntos
fcart xs ys = [(x,y) | x <- xs, y <- ys]

--7. Una función que reciba un conjunto y un elemento y devuelva True si
--el elemento pertenece al conjunto, falso en otro caso.
partede x xs = or [y == x | y <- xs]

--8. Una función que reciba dos conjuntos y devuelva la intersección de
--los mismos
inter xs ys = [y | x <- xs, y <- ys, x == y]

--9. Una función que reciba un elemento un conjunto y elimine el elemento 
--del conjunto
elim x xs = [y | y <- xs, x /= y]

--10. Una función que implemente la unión de dos conjuntos.
union xs ys = [x | x <- xs++ys]

--11. Una función que reciba una matriz y devuelva su diagonal principal
diagop xss = [ (!!y)((!!y) xss) | y <- [0..(mylength xss)-1]]

--transpuesta de una matriz
transpo xss = [ mymap (!!y) xss | y <- [0..(mylength xss)-1]]

--12. Una función que reciba una matriz y devuelva su diagonal secundaria
diagos xss = reverse[(!!y)((!!y) (reverse xss)) | y <- [0..(mylength xss)-1]]

--13. Una función que reciba una matriz y devuelva True si esta es un
--cuadrado perfecto
cupe xss = and[y | x <- [mymap (mylength) xss], y <- mymap(==mylength x) x ]

--14. Una función que multiplique 2 matrices
multmat xss yss = 

--15. Una función que reciba un número y devuelva True si es primo
--16. zipWith
--17. zip3 (hace lo mismo que zip pero con 3 listas, devuelve una lista
--de triplas)
--18. zip4 (hace lo mismo que zip pero con 4 listas, devuelve una lista 
--de tuplas de 4 elementos)
--19. Definir una función que reciba una lista y una función de orden y 
--devuelva la verdad si la lista esta ordenada de acuerdo a una función
--de orden
--II. Evaluar las siguientes expresiones (hacer en
--papel y verificar en la computadora):
--1. [x|x<-[y|y<-[2,3,4,5], even y], odd x]
--2. [[1|x<-[2|y<-[1..10]],even x]|z<-[1..5]]
--3. [even x|x<-[y|y<-[1..20],y>5],odd x]
--4. [x|x<-[[[1,2,3,4]]],x<-x,x<-x]
--5. [[x|x<-[1,2,3],x<-y]|y<-[[‘a’,’b’,’c’],[‘d’,’e’],[‘h’,’i’,’j’]]
--6. [[x<-[1,2,3,4]]| x<-[1,2,3], y <-[2,3]]
--7. [[x<-[1,2,3,4]]| x<-[1,2,3], y <-[]]
--8. [[x<-[1,2,3,4]]| x<-[1,2,3], y <-[2..x]]