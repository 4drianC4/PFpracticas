--I. Definir en términos de foldr:
--1. Una función que reciba una lista y devuelva la productoria de sus
-- elementos
multlis xs = foldr (*) 1 xs

--2. map
mymap f rs= foldr (fun) [] rs
    where 
        fun x xs = f x:xs

--3. filter
myfilter f rs = foldr (fun) [] rs
    where
        fun x xs = if(f x == True)then x:xs else xs

--4. length
mylength rs = foldr (f) 0 rs
    where
        f x y = 1+y

--5. (++)
myunion xs ys = foldr (:) ys xs

--6. reverse
myreverse xs = foldr (f) [] xs
    where
        f l ls= ls++[l]

--7. concat
myconcat xss = foldr (f) [] xss
    where 
        f rs ls = foldr (:) ls rs

--8. takeWhile
mytakewhile c xs = foldr(f) [] xs
    where
        con l = 
            if(c l== True) then True
            else False
        f l ls = 
            if(con l == True) then l:ls
            else ls

--9. Una función que reciba una lista de dígitos y devuelva el Nro. que se 
-- forma al juntarlos.
juntar ns = foldl (f) 0 ns
    where 
        f y x = (y*10+x)

--10. Una función que encuentre el valor mínimo de una lista
nmin xs = foldr (min) x xs
    where
        x = head xs
        min x l = if (x > l) then l else x

--11. Una función que reemplace los elementos de una lista por el valor
--mínimo de esa lista.
nrep xs = foldr (ret) [] xs
    where
        rem = (nmin xs)
        ret x xs = rem:xs

--12. Una función que reciba una lista y retorne true si la lista está
--ordenada ascendentemente, false en otro caso.


--II. Sea la definición de la función foldl:
--foldl () a [] = a
--foldl () a [x1, x2, x3..xn] =(...( (a  x1)  x2)..)  xn
--La misma definición utilizando función en lugar de operador, sería:
--foldl f a [] = a
--foldl f a [x1, x2, x3..xn] =f(...f (f (f a x1) x2) x3 ...) xn
--Esta función es similar a foldr, sin embargo a diferencia de foldr que
--trabaja hacia la derecha, foldl trabaja hacia la izquierda (foldl= fold
-- left, foldr=fold right)
--1. Definir el tipo de foldl
--2. Hacer las anterior definiciones (1 al 12) utilizando foldl en lugar 
--de foldr.

