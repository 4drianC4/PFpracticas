--PRÁCTICA Recursividad
--1. Definir una función que compare 2 listas y devuelva True si las 
--listas son iguales
comp [] ys = []
comp xs [] = []
comp (x:xs) (y:ys) = if((length xs) == (length ys)) then (x == y):(comp xs ys) 
                    else [False]
listiguales xs ys = and (comp xs ys)

--2. Definir una función que fusione 2 listas ordenadas en una 3ra. 
--ordenada (sin necesidad de ordenar).

ord [] ys = []
ord (x:xs) ys = x:(ord xs ys)


--3. Definir una función que verifique si una lista de listas podría ser 
--considerada una matriz

mylength [] = 0
mylength (x:xs) = (mylength xs) + 1

vmat [] = []
vmat (x:xs) = if(mylength xs > 0)then (((mylength x) == mylength (head xs)):vmat xs)
                else True:vmat xs

verificarmat xs = and (vmat xs) 

--4. Definir una función que reciba 1 matriz y una función de orden y 
--devuelva True si la matriz esta ordenada de acuerdo a la función de 
--orden.

--orden filas
ordf [] c = True
ordf [_] c = True
ordf (x:y:xs) c = (c x y)&&(ordf (y:xs) c)

--concat 
myconcat [] = []
myconcat (xs:xss) = (xs)++(myconcat xss)

--orden matrices
ordm (xss) c = (ordf (myconcat xss) c)

--5. Definir una función que reciba una lista de números y devuelva todos
--los números pares

pars [] = []
pars (x:xs) = if (mod x 2 == 0) then x:pars xs else pars xs 

--6. Definir una función que reciba una lista de listas y devuelva solo 
--aquellas cuya longitud sea par.

longpars [] = []
longpars (xs:xss) = if (f == 0) then xs:longpars xss else longpars xss 
    where f = mod (mylength xs) 2

--7. Definir una función que reciba una lista de listas de números y 
--borre todos los números pares de estas listas

nopars [] = []
nopars (x:xs) = if (mod x 2 /= 0) then x:nopars xs else nopars xs 
noparss [] = []
--nopars [_] = []
noparss (xs:xss) = if (mod x 2 /= 0) then ((nopars xs):noparss xss) else (noparss xss)
    where x = head (xs) 

--8. Definir una función que reciba una lista de listas y devuelva una 
--lista formada por los penúltimos elementos de las listas



--9. Definir una función que reciba un número y devuelva una lista con los
--posibles divisores del número.



--10. Definir una función que busque un elemento en una lista mediante 
--búsqueda secuencial (la función debería devolver la posición donde se
--encuentra el elemento).



--11. Definir una función que busque un elemento en una lista mediante 
--búsqueda binaria (la función debería devolver la posición donde está el
--elemento).
--12. Definir una función que realice el ordenamiento de una lista de
--números por el método de Selección Directa
--13. Definir una función que realice el ordenamiento de una lista de
--números por el método de Inserción Directa
--14. Definir una función que realice el ordenamiento de una lista de
--números por el método de Intercambio Directo o Burbuja
--15. Definir una función que realice el ordenamiento de una lista de
--números por el método de QuickSort.
--16. Definir una función que reciba una matriz y devuelva su transpuesta
--17. Definir una función que reciba 2 matrices y las multiplique
--18. Definir una función que reciba 3 matrices y las multiplique
--19. Definir una función que reciba 4 matrices y las multiplique
--20. Definir una función que reciba una lista de matrices y retorne su 
--productoria
--21. Definir una función (f xs ys ) que verifique si la lista xs está 
--incluida en la lista ys, devolviendo verdadero o falso según caso.
--22. Definir una función (g xs ys ) que devuelva las posiciones de inicio
--donde xs está en ys. Por ejemplo: 
--g [1,1,1] [3,1,1,1,1,4,1,1,3,1,1,1,7] => [1,2,9]


--definir la función suma de dos números naturales y devuelva el resultado
--suma::Natural -> Natural -> Natural