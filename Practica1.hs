--EJERCICIOS 1 

--1.Definir una función que reciba el lado de un cuadrado y devuelva 
--su área.
areaCuadrado l = l * l

--2y3.Definir una función que reciba la base y la altura de un rectángulo 
--y devuelva su área y su perímetro.
rectanguloAP b a = (b * a, b*2 + a*2)

--4.Definir una función que reciba 2 números y devuelva verdad si el 
--primero es mayor que el segundo.
mayorq a b = a > b

--5.Definir una función que reciba un número y retorne verdad si este 
--es múltiplo de 2.
multiplo2 :: Integral a => a -> Bool
multiplo2 x = mod x 2 == 0

--6.Definir una función que reciba un número y devuelva verdad si este 
--es múltiplo de 2 y de 3 al mismo tiempo.
multiplo2y3 x = mod x 2 == 0 && mod x 3 == 0

--7.Definir una función que reciba un número y lo devuelva elevado a 
--la potencia 3.
potencia3 x = x^3

--8.Definir funciones que reciban un número y lo devuelvan elevado a 
--la potencia 4,8,10,32.
potencia4 x = x^4
potencia8 x = x^8
potencia10 x = x^10
potencia32 x = x^32

--9.Definir una función que reciba dos números y una función de orden
--y devuelva verdad si los números obedecen a la función de orden, 
--falso en otro caso.
funcionOrd :: p -> p
funcionOrd f = f

--EJERCICIOS 2(Por distincion de casos)

--1.Definir una función que devuelva el mayor de 2 números
mayor2 x y = if x > y then x else y

--2.Definir una función que reciba 3 números y devuelva el mayor
mayor3 x y z = if x > y && x > z then x
                else if y > x && y > z then y
                else z

--3.Definir una función que reciba 4 números y devuelva el mayor
mayor4 w x y z = if w > x && w > y && w > z then w
                    else if x > w && x > y && x > z then x
                    else if y > w && y > x && y > z then y
                    else z

--4.Definir una función que reciba dos exámenes parciales, un final
--y una segunda instancia y devuelva el mensaje “Aprobado”, “Reprobado” 
--o “Abandono” según el caso.
promedio pp sp fn si = if ((pp + sp)/2)>50 || fn > 50 || si > 50 then "Aprobado"
                        else if ((pp + sp)/2)== 0 && fn == 0 && si == 0 then "Abandono"
                        else "Reprobado"

--5.Definir una función que reciba 2 fechas y devuelva la fecha mayor
fechaMayor (a1,m1,d1) (a2,m2,d2) =
                        if (a1 > a2) then (a1,m1,d1)
                        else if(m1 > m2 && a1 == a2) then (a1,m1,d1)
                        else if(d1 > d2 && m1 == m2 && a1 == a2) then (a1,m1,d1)                        
                        else(a2,m2,d2)

--EJERCICIOS 3

--1.Definir una función que reciba 4 número y devuelva el mayor.
--Por combinación
mayor4v2:: Int -> Int -> Int -> Int -> Int
mayor4v2 a b c d = mayor4 a b c d
--Por distinción de casos
mayor4v3 w x y z
    |w > x && w > y && w > z = w
    |x > w && x > y && x > z = x
    |y > w && y > x && y > z = y
    |otherwise = z

--2.Definir una función que reciba una nota y devuelva el mensaje 
-- “Aprobado” o “Reprobado”.
nota1 n
    |n > 50 && n <= 100 = "Aprobado"
    |n < 50 && n > 0 = "Reprobado"
    |otherwise = "no es una nota correcta"

--3.Definir una función que reciba una nota y devuelva el mensaje 
-- “Excelente“ si la nota esta entre 90-100, “Bien” si esta entre 70-89,
-- “Regular” si esta entre 51-69 y mal si esta entre 0-50.
nota2 n
    |n <= 100 && n >= 90 = "Exelente"
    |n < 90 && n >= 70 = "Bien"
    |n < 70 && n >= 51 = "Regular"
    |otherwise = "mal"

--4.Definir una función que reciba como argumentos las notas de primer 
--parcial, segundo parcial, final y segunda instancia y retorne el 
--mensaje aprobado o reprobado, según el caso.
nota3 pp sp fn si 
    |((pp+sp)/2) > 50 || fn > 50 || si > 50 = "Aprobado"
    |((pp+sp)/2) == 0 && fn == 0 && si == 0 = "Abandono"
    |otherwise = "Reprobado"

--5.Definir una función que reciba 16 números y retorne el mayor
mayor16 a b c d e f g h i j k l m n o p = 
    (mayor4v3 (mayor4v3 a b c d) (mayor4v3 e f g h) (mayor4v3 i j k l) (mayor4v3 m n o p))

--6. Definir una función que reciba un quebrado y devuelva verdad si 
--este es mayor que 1 y falso en otro caso
mayorq1 q = q > 1

--7.Definir una función que reciba 2 fechas y devuelva la fecha mayor
fechaMayorv2 (a1,m1,d1) (a2,m2,d2) 
    |(d1 >= d2 && m1 >= m2 && a1 >= a2) = (a1,m1,d1)
    |(d1<=d2 && m1 >= m2 && a1 >= a2) = (a1,m1,d1)
    |(d1<=d2 && m1 <= m2 && a1 >= a2) = (a1,m1,d1)                        
    |otherwise = (a2,m2,d2)

--8.Definir una función que reciba 2 fechas y devuelva los años 
--transcurridos
aTran (a1,m1,d1) (a2,m2,d2)
    |a1 > a2 = a1 - a2
    |a1 < a2 = a2 - a1
    |otherwise = 0

--9.Definir una función que reciba 2 fechas y devuelva los meses 
--transcurridos
mTran (a1,m1,d1) (a2,m2,d2)
    |m1 >= m2 = anios + (m1 - m2)
    |m1 < m2 = anios + (m2 - m1)
    |otherwise = 0
    where anios = (aTran (a1,m1,d1) (a2,m2,d2)) *12

--10.Definir una función que reciba 2 fechas y devuelva los días 
--transcurridos
dTran (a1,m1,d1) (a2,m2,d2)
    |d1 >= d2 = meses + (d1 - d2)
    |d1 < d2 = meses + (d2 - d1)
    |otherwise = 0
    where meses = (mTran (a1,m1,d1) (a2,m2,d2)) *30
--11.Definir una función que reciba 2 fechas y devuelva los días, 
--meses y años transcurridos
fTran (a1,m1,d1) (a2,m2,d2)
    |a1 >= a2 && m1 >= m2 && d1 >= d2 = (a1-a2, m1-m2, d1-d2)
    |a1 >= a2 && m1 >= m2 && d1 <= d2 = (a1-a2, m1-m2, d2-d1)
    |a1 >= a2 && m1 <= m2 && d1 >= d2 = (a1-a2, m2-m1, d1-d2)
    |a1 <= a2 && m1 >= m2 && d1 >= d2 = (a2-a1, m1-m2, d1-d2)
    |a1 <= a2 && m1 <= m2 && d1 >= d2 = (a2-a1, m2-m1, d1-d2)
    |a1 <= a2 && m1 >= m2 && d1 <= d2 = (a2-a1, m1-m2, d2-d1)
    |a1 >= a2 && m1 <= m2 && d1 <= d2 = (a1-a2, m2-m1, d2-d1)
    |a1 <= a2 && m1 <= m2 && d1 <= d2 = (a2-a1, m2-m1, d2-d1)
    |otherwise = (0,0,0)

--12.Definir una función que reciba un instante (fecha, hora) e 
--incremente el instante en 1 segundo.
ins1 (a,m,d,h,min,s) 
    |s == 59 && min < 59 = (a,m,d,h,min+1,00)
    |s == 59 && min == 59 && h < 23 = (a,m,d,h+1,00,00)
    |mod a 4 == 0 && m == 2 && d == 29 = (a,m+1,1,00,00,00) --año bisiesto
    |h == 23 && d < 30= (a,m,d+1,00,00,00)
    |(d == 30 || d == 31) && m < 12 = (a,m+1,1,00,00,00) 
    |m == 12 = (a+1,1,1,00,00,00)
    |otherwise = (a,m,d,h,min,s+1)

--EJERCICIOS 4 (Usando case)

--1. Definir una función que reciba una vocal y retorne la siguiente
sigvoc:: Char -> Char
sigvoc v =
    case v of
       'a' -> 'e'
       'e' -> 'i'
       'i' -> 'o'
       'o' -> 'u'
       'u' -> 'a'
       _ -> '?'

--2. Definir una función que reciba un dígito y retorne su literal
diglit:: Int -> String
diglit d =
    case d of
        1 -> "uno"
        2 -> "dos"
        3 -> "tres"
        4 -> "cuatro"
        5 -> "cinco"
        6 -> "seis"
        7 -> "siete"
        8 -> "ocho"
        9 -> "nueve"
        0 -> "cero"
        _ -> "?"

--3. Suponiendo que representamos los valores lógicos por 1 y 0 (true y 
--false), definir una función que reciba dos valores lógicos y retorne 
--el resultado de aplicarle la operación and
myand q r =
    case (q,r) of
        (1,1) -> 1
        (_,_)-> 0

--4. Idem a 3 pero para or
myor q r =
    case (q,r) of
        (0,0) -> 0
        (_,_)-> 1
--5. Idem a 3 para xor
myxor q r =
    case (q,r) of
        (1,1) -> 0
        (0,0) -> 0
        (_,_) -> 1
--6. Idem a 3 pero que reciba como argumento la operación que se realizará.
arlog ar = ar
--7. Definir una función que reciba un número de dos dígitos y retorne su
--literal
diglit2 x =    
    case (d2,d1) of
        (1,0) -> "diez"
        (1,1) -> "once"
        (1,2) -> "doce"
        (1,3) -> "trece"
        (1,4) -> "catorce"
        (1,5) -> "quince"
        (1,_) -> concat ["dieci", diglit d1]
        (2,0) -> "veinte"
        (2,_) -> concat["veinti",diglit d1]
        (3,0) -> "trenta"
        (3,_) -> concat["treintai",diglit d1] 
        (4,0) -> "cuarenta"
        (4,_) -> concat["cuarentai",diglit d1]
        (5,0) -> "cincuenta"
        (5,_) -> concat["cincuentai",diglit d1]
        (6,0) -> "sesenta"
        (6,_) -> concat["sesentai",diglit d1]
        (7,0) -> "setenta"
        (7,_) -> concat["setentai",diglit d1]
        (8,0) -> "ochenta"
        (8,_) -> concat["ochentai",diglit d1]
        (9,0) -> "noventa"
        (9,_) -> concat["noventai",diglit d1]
        _ -> case x < 10 && x > 100 of
                _ -> diglit x
    where
        d1 = mod x 10
        d2 = div x 10
--8. Definir una función que reciba un número de tres dígitos y retorne su
--literal
    --LARGO

--9. Definir una función que reciba dos números y retorne el menor
menorv2 n1 n2 = 
    case (n1 >= n2) of
        False -> n1
        True -> n2

--10. Definir una función que reciba 6 números y devuelva el menor
menor6 n1 n2 n3 n4 n5 n6 =
    case (n1 <= n2 && n1 <= n3 && n1 <= n4 && n1 <= n5 && n1 <= n6) of
        True -> n1
        false -> case (n2 <= n1 && n2 <= n3 && n2 <= n4 && n2 <= n5 && n2 <= n6) of
                    True -> n2
                    False -> case (n3 <= n1 && n3 <= n2 && n3 <= n4 && n3 <= n5 && n3 <= n6) of
                                True -> n3
                                False -> case (n4 <= n1 && n4 <= n2 && n4 <= n3 && n4 <= n5 && n4 <= n6) of
                                            True -> n4
                                            False -> case (n4 <= n1 && n4 <= n2 && n4 <= n3 && n4 <= n5 && n4 <= n6) of
                                                        True -> n4
                                                        False -> case (n5 <= n1 && n5 <= n2 && n5 <= n3 && n5 <= n4 && n5 <= n6) of
                                                                    True -> n5
                                                                    False -> case (n6 <= n1 && n6 <= n2 && n6 <= n3 && n6 <= n4 && n6 <= n5) of
                                                                                True -> n6

--11. Definir una función que reciba 3 números y devuelva el mensaje 
-- “Sumatoria mayor” si la sumatoria de los números es menor que 20, el 
--mensaje “Sumatoria menor” si la sumatoria es menor que 10 y el mensaje
-- “Vacio” en otro caso.
sumatoriav2 n1 n2 n3 =
    case (n1+n2+n3) < 10 of
        True -> "sumatoria menor"
        False -> case((n1+n2+n3) < 20) of
                    True -> "sumatoria mayor"
                    False -> "vacio"

--12. Definir una función que reciba 3 notas que devuelva el mensaje 
-- “Excelente“ si el promedio esta entre 90-100, “Bien” si esta entre 70-89,
-- “Regular” si esta entre 51-69 y mal si esta entre 0-50.
notac pp sp tp =
    case (pp + sp)/2 >= 90 || tp >= 90 of
        True -> "Exelente"
        False -> case (pp + sp)/2 >= 70 || tp >= 70 of
                    True -> "Bien"
                    False -> case (pp + sp)/2 >= 51 || tp >= 51 of
                                True -> "Regular"
                                False -> case (pp + sp)/2 >= 0 || tp >= 0 of
                                    True -> "Mal"
                                    False -> "?"
--EJERCICIO 6 (Definiciones locales)

--13. Definir una función que reciba 6 números y devuelva el menor
menor6v2 n1 n2 n3 n4 n5 n6 =
    let
        m1 = if (n1 < n2) then n1 else n2
        m2 = if (n3 < n4) then n3 else n4
        m3 = if (n5 < n6) then n5 else n6
    in
        menorv2 m1 (menorv2 m2 m3)
--14. Definir una función que reciba 3 números y devuelva el mensaje 
--Sumatoria mayor” si la sumatoria de los números es menor que 20, el 
--mensaje “Sumatoria menor” si la sumatoria es menor que 10 y el mensaje 
-- “Vacio” en otro caso.
sumatoriav3 n1 n2 n3 = 
    if(r1 && n1+n2+n3 > 0)then "sumatoria menor" 
    else if(r2 && n1+n2+n3 > 10)then "sumatoria mayor" 
    else "vacio"   
    where
        r1 = if (n1+n2+n3 < 10) then True else False
        r2 = if (n1+n2+n3 < 20) then True else False

--15. Definir una función que reciba 3 notas que devuelva el mensaje 
-- “Excelente“ si el promedio esta entre 90-100, “Bien” si esta entre 70-89,
-- “Regular” si esta entre 51-69 y mal si esta entre 0-50.
--inservible

--16. Inventar 3 ejercicios que muestren la utilidad de las definiciones
-- locales

--EJERCICIO 7 (Reconocimiento de patrones)

--1. Definir una función que reciba una fecha y devuelva el día
devdia f@(d,m,a) = a

--2. Definir una función que reciba una fecha y devuelva el mes
devmes f@(d,m,a) = m

--3. Definir una función que reciba una fecha y devuelva el año
devanio f@(d,m,a) = a

--4. Definir una función que reciba 2 quebrados y devuelva el mayor
quebmay q1@(q1n,q1d) q2@(q2n,q2d) = if(d1 >= d2) then q1 else q2
    where
        d1 = q1n/q1d
        d2 = q2n/q2d

--5. Definir una función que reciba 1 quebrado y lo devuelva reducido
--redqueb q@(n1,n2) = 

--6. Definir una función que reciba 1 quebrado y devuelva su signo como 
--carácter
sigqueb q@(n1,n2) = if (n1 < 0 && n2 < 0)||(n1 > 0 && n2 > 0)then '+' else '-'

--7. Definir una función que reciba 3 fechas y devuelva la fecha menor
fechmay2 f1@(d1,m1,a1) f2@(d2,m2,a2)
    |(d1 >= d2 && m1 >= m2 && a1 >= a2) = f1    
    |(d1 <= d2 && m1 >= m2 && a1 >= a2) = f1
    |(d1 <= d2 && m1 <= m2 && a1 > a2) = f1                    
    |otherwise = f2

fechmay3 f1@(d1,m1,a1) f2@(d2,m2,a2) f3@(d3,m3,a3) = fechmay2 (fechmay2 f1 f2) f3 

--8. Definir una función que reciba 2 horas y devuelva la hora mayor
horamay ho1@(h1,min1,s1) ho2@(h2,min2,s2)
    |(h1 > h2 && min1 > min2 && s1 > s2) = ho1    
    |(h1 > h2 && min1 > min2 && s1 > s2) = ho1
    |(h1 > h2 && min1 < min2 && s1 > s2) = ho1                    
    |otherwise = ho2
--9. Definir una función que reciba 2 instantes y devuelva el mas reciente
--(Instante es una fecha, hora, minuto, segundo)
insta2 i1@(ho1@(h1,min1,s1),f1@(d1,m1,a1)) i2@(ho2@(h2,min2,s2),f2@(d2,m2,a2))
    |horamay ho1 ho2 == ho1 && fechmay2 f1 f2 == f1 = i1
    |horamay ho1 ho2 == ho2 && fechmay2 f1 f2 == f1 = i1
    |otherwise = i2

--10. Definir una función que reciba un número natural y devuelva el
-- siguiente


--11. Definir una función que reciba un quebrado quebrados y devuelva la 
--simplificación (modelar el quebrado de quebrados como par de pares 
--((a,b),(c,d))