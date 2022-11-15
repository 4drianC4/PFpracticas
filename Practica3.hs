--1. Definir una función que reciba una lista de listas y devuelva el 
--1er. elemento de la 1ra. lista
lis1:: [[a]] -> a
lis1 xss = head (head xss) 

--2. Definir una función que reciba una lista de listas de funciones y un
--elemento y aplica la 1ra función de la primera lista al elemento
lis2:: [[t1 -> t2]] -> t1 -> t2
lis2 fss x = head (head fss) x

--3. Definir una función que reciba una lista de elementos y devuelva el 
--segundo
lis3:: [t] -> t
lis3 x = head (tail x)


--4. Definir una función que reciba una lista de elementos y devuelva el 
--tercero
lis4:: [t] -> t
lis4 x = head(tail (tail x))

--5. Definir una función que reciba una lista de listas y devuelva el 5to.
--Elemento de la 3ra. lista.
lis5:: [[t]] -> t
lis5 xss =  ((xss)!!2)!!4

--6. Definir una función que reciba una lista de listas de listas y 
--devuelva el 3er. elemento de la 4ta. Lista de la 2da. lista
lis6:: [[[t]]] -> t
lis6 x = head(drop 2 (head(drop 3 (head(drop 1 x)))))

--7. Definir una función que verifique si una lista esta ordenada de 
--acuerdo a una función de orden.
--lis7:: t2 -> [t1 -> t2] -> Bool
lis7 fo l = and(map (l) (fo))
--8. Definir una función que compare 2 listas y devuelva True si las
--listas son iguales
lis8 l1 l2 = and(zipWith (==) l1 l2)

--9. Definir una función que verifique si una lista de listas podría ser 
--considerada una matriz
lis9 l = and(map (==lr)(lm))
    where 
        lm = map length l
        lr = head lm

--10. Definir una función que reciba un número y una lista y devuelva el
--elemento de la lista que esta en la posición n
lis10 n l = l!!(n-1)

--11. Definir las funciones length, filter, zip utilizando las otras 
--funciones
miLength l =  sum(map (^0) l)
miFilter x l = x

--12. Definir una función que reciba una matriz y devuelva su transpuesta
--lis12 :: [[c]] -> [[c]]
mat1 = [[1,2,3],[4,5,6],[7,8,9]]

transpuesta :: [[a]] -> [[a]]
transpuesta mss = map (obtenerColumna mss)[0..ultimaCol]
  where
    ultimaCol = (length (head mss))  -1
    obtenerColumna mss c = map (!!c) mss
              
transpuesta1 ([]:_) = []
transpuesta1 x = (map head x) : transpuesta1 (map tail x)
--13. Definir una función que reciba 2 matrices y las multiplique
doCasilla fs cs = sum(zipWith (*) fs cs)
doFila css fs  = map (doCasilla fs) css
doMat fss css = map (doFila css) fss
multiplicacion ass bss = doMat ass (transpuesta bss)
--14. Definir una función que reciba 3 matrices y las multiplique
--15. Definir una función que reciba 4 matrices y las multiplique
--16. Definir una función que reciba 1 matriz y una función de orden y 
--devuelva True si la matriz esta ordenada de acuerdo a la función de orden.
--ordMat :: (a -> a-> Bool) -> [[a]] -> Bool
--ordMat :: Foldable t => (a -> Bool) -> t [a] -> Bool
ordMat f xss = and(map (f) (concat xss))



estaOrdenada fo xs = and (zipWith fo xs (tail xs))

esMatrizOrdenada fo mss= estaOrdenada fo (concat mss)

esMatrizOrdenada2 fo mss = (and(map (estaOrdenada fo) mss)) && (and(zipWith fo colas (tail cabezas)))
  where 
    cabezas = map head mss
    colas = map last mss
--17. Definir una función que reciba una lista de números y devuelva todos 
--los números pares
onPar xs = filter(comp) xs
  where comp x = (mod x 2)==0

--18. Definir una función que reciba una lista de listas y devuelva una 
--lista de sus longitudes.
longll xss = map(length ) xss

--19. Definir una función que reciba una lista de listas y devuelva solo 
--aquellas cuya longitud sea par.
longpar xss = onPar(longll xss)

--20. Definir una función que reciba una lista de listas de números y 
--borre todos los números pares de estas listas

lisimp xss = map (f) xss
  where 
    rm x = (mod x 2) /= 0
    f xs = filter (rm) xs

--21. Definir una función que reciba una lista de listas y devuelva una 
--lista formada por los penúltimos elementos de las listas
pen xss = map (last.init) xss

--22. Definir una función que reciba un número y devuelva una lista con los 
--posibles divisores del número.
listdiv x = filter (cap x) [1..x]
  where cap x xss = (mod x xss) == 0

--23. Definir la función zipWith en terminos de zip


-----------------------------------------------------Parte 2
estaOrd xs = foldr f a xs
 where
  a=(last xs,True)
  f anterior (sgte, esta)=
           if esta
             then (anterior, anterior<=sgte)
              else (anterior, False)


--estaOrd xs = foldr f a xs
obtenerSigno (n,d)= if (div n d)<0 then '-' else '+'

miConcat xss = foldr f a xss
                where
                  a = []
                  f =(++)




matC=[[1,2,5],
      [4,9,10],
      [20,40,50]]


matA = [[3,    4,    9],
        [1,    2,    7],
        [6,    8,    3],
        [5,    4,    1]
       ]
matB=
 [[1,    2    ,3,    4,    5],
  [10,    20,    30,    40,    50],
  [100,    200,300,400,500]
 ]

f g x y = g ((x +1) , ( y True))


--EXAMEN PRIMER PARCIAL
mif3 g x y = (g ((x +1) , ( (y True)++"xxx"))) + 20
mif3::((Int,String) -> Int) -> Int -> (Bool -> String) -> Int

 
mif2 g x y = g ((x +1) , ( y True))
mif2::((Int,try) -> trg) -> Int -> (Bool -> try) -> trg

--insertar os fec = fechasMenores ++(fec:fechasMayores)
insertar os fec = concat[fechasMenores ,[fec],fechasMayores]
  where
   fechasMenores = takeWhile (esFechaMayor fec) os
   fechasMayores = dropWhile (esFechaMayor fec) os
   esFechaMayor (d1,m1,a1) (d2,m2,a2)
    = a1>a2 ||
      (a1==a2 && m1>m2)||
      (a1==a2 && m1==m2 && d1>d2)

getDiaMes xs year = foldr f a xs
 where
  a= [ ]
  f (d,m,a) rs = if a==year then (d,m):rs else rs

fs= [(2,3,17),(12,5,18), (4,6,17),(9,2,17),(8,8,16),(5,7,17)]
ps=[(1,2,3),(1,5,3),(2,2,4),(3,3,5),(4,4,5)]

mif x y g = g (x+1,y 5)

mif:: Int -> (Int -> try ) -> ((Int , try ) -> trg ) -> trg

fun11 x y z w u
      |x (&&) y = (\a -> a>2)
      |z && w = u

fun22 x y = x y


cambio xs f=foldr f a xs     
  where a = []


--EXAMEN PRIMER PARCIAL
mif3 g x y = (g ((x +1) , ( (y True)++"xxx"))) + 20
mif3::((Int,String) -> Int) -> Int -> (Bool -> String) -> Int

 
mif2 g x y = g ((x +1) , ( y True))
mif2::((Int,try) -> trg) -> Int -> (Bool -> try) -> trg

--insertar os fec = fechasMenores ++(fec:fechasMayores)
insertar os fec = concat[fechasMenores ,[fec],fechasMayores]
  where
   fechasMenores = takeWhile (esFechaMayor fec) os
   fechasMayores = dropWhile (esFechaMayor fec) os
   esFechaMayor (d1,m1,a1) (d2,m2,a2)
    = a1>a2 ||
      (a1==a2 && m1>m2)||
      (a1==a2 && m1==m2 && d1>d2)

getDiaMes xs year = foldr f a xs
 where
  a= [ ]
  f (d,m,a) rs = if a==year then (d,m):rs else rs

fs= [(2,3,17),(12,5,18), (4,6,17),(9,2,17),(8,8,16),(5,7,17)]
ps=[(1,2,3),(1,5,3),(2,2,4),(3,3,5),(4,4,5)]

mif x y g = g (x+1,y 5)

mif:: Int -> (Int -> try ) -> ((Int , try ) -> trg ) -> trg

fun11 x y z w u
      |x (&&) y = (\a -> a>2)
      |z && w = u

fun22 x y = x y

fun33::(Char -> Int) -> ((Bool -> Int)-> Char) -> (Char -> Int) 
fun33 fx fy = fx
  where
    fx c1 = 2
    fy b n2 = 'a'

exa xs ys = saca xs ys
  where
    pos xs ys = length( takeWhile (==True) (zipWith (==) xs ys))
    saca xs ys = (pos,(!!(pos xs ys)) xs, (!!(pos xs ys)) ys)

descomprimir xs = concat(map g xs)
  where
    g (c,n) = map (rem c) [0..n-1]
    rem x n = x 

listarar ls ar = div (soloAl (filar ls ar)) (tamfilar ls ar)
  where
    soloar ar2 l@(_,_,ar1,numAl,_) = ar1 == ar2
    filar ls ar = filter (soloar ar) ls
    tamfilar ls ar = length (filar ls ar)
    sacAl l@(_,_,ar,numAl,_) = numAl
    soloAl ls = (sum (map sacAl ls))

ms = [("INF100", "TallerI","Prg",55,20),
    ("INF101","ProgramacionI","Prg",60,30),
        ("MAT202","CalculoII","Ex",70,35),
            ("MAT102","CalculoI","Ex",80,45),
                ("ADM102","AdministracionII","An",90,50)]

mat = ("INF106","TallerI","Prg",72,32)

procesar ls ar = div (sum (map fun2(filter(fun1 ar)ls))) (length (filter (fun1 ar)ls))
  where
    fun1 = (\ar -> \(a,b,c,d,e) -> ar == c)
    fun2 = (\(a,b,c,d,e) -> d)

--fun :: (Ord a, Num a) =>((Char -> Bool) -> Bool)-> (Bool -> Bool, a) -> (Bool -> Bool, a) -> (Bool -> Bool, a)
--fun x y@(z,w) s 
--      |x(\a -> a == 's') = s
--      |z((>2)w) = y