--PRÁCTICA SOBRE TIPOS SINÓNIMOS Y TIPOS ENUMERADOS

--I. Definir los siguientes tipos de datos
-- *ZonaGeografica que permita representar las 3 zonas geográficas de
--  Bolivia (valles, llanos y altiplano).
data ZonaGeografica = Valles | Llanos | Altiplano 
    deriving Show

-- *Departamento que permita representar los 9 departamentos de Bolivia.
data Departamentos = Cochabamba | LaPaz | SantaCruz | Oruro | Beni | Pando | Potosi | Tarija | Sucre 
    deriving Show

--Utilizando estos tipos definir las sgtes funciones:
-- 1.Una función que reciba un zona y devuelva un mensaje indicando sus
--   características

caracteristicas:: ZonaGeografica -> String
caracteristicas Valles = "frio y calor, mucha vegetacion"
caracteristicas Llanos = "clima calido, mucha humedad"
caracteristicas Altiplano = "mucho frio, clima seco"

-- 2.Una función que reciba un departamento y devuelva True si pertenece 
--   a la zona de los valles, falso en otro caso.

zonaValles:: Departamentos -> Bool
zonaValles Cochabamba = True
zonaValles Sucre = True
zonaValles Tarija = True
zonaValles _ = False 

zonaLlanos:: Departamentos -> Bool
zonaLlanos Pando = True
zonaLlanos Beni = True
zonaLlanos SantaCruz = True
zonaLlanos _ = False 

-- 3.Una función que reciba un departamento y devuelva la zona a la que
--   corresponde el departamento.

zonaPer:: Departamentos -> ZonaGeografica
zonaPer Cochabamba = Valles
zonaPer Tarija = Valles
zonaPer Sucre = Valles
zonaPer SantaCruz = Llanos
zonaPer Beni = Llanos
zonaPer Pando = Llanos
zonaPer LaPaz = Altiplano
zonaPer Oruro = Altiplano
zonaPer Potosi = Altiplano

--4. Una función que reciba un lista de departamentos y devuelva aquellos
--   que pertenecen a la zona de los llanos o de los valles.
type ListaDepartamentos = [Departamentos]
zonaLV:: ListaDepartamentos -> ListaDepartamentos
zonaLV [] = []
zonaLV (x:xs) = if((zonaValles x == True) || (zonaLlanos x == True))then x:zonaLV xs else zonaLV xs

listaDep = [Cochabamba, LaPaz, SantaCruz, Oruro, Beni, Pando, Potosi, Tarija, Sucre]

--II. Sean las siguientes definiciones de tipo:
type Dia = Int
type Mes= Int
type Anio = Int
type Fecha=(Dia,Mes,Anio)
type Periodo=(Fecha,Fecha)
type Nombre=String
type Presidente=(Nombre,Periodo)
--el tipo Presidente es un par que representa el Nombre del presidente
--y el período de tiempo en que gobernó.
--Definir :

--1. Una función que reciba un Periodo y devuelva el tiempo transcurrido
--   en años.

ttran::Periodo -> Anio
ttran (f1@(_,_,a1),f2@(_,_,a2)) = a1 - a2 

--2. Una función que reciba un Presidente y devuelva el tiempo total en 
--   años que gobernó.
tpr::Presidente -> Anio
tpr (n,(f1@(_,_,a1),f2@(_,_,a2))) = ttran (f1,f2)

--3. Definir una función que reciba dos presidentes y devuelva aquel que
--   gobernó más tiempo.

gob:: Presidente -> Presidente -> Presidente
gob p1@(n1,(f1@(_,_,a1),f2@(_,_,a2))) p2@(n2, (f3@(_,_,a3),f4@(_,_,a4))) = if (tpr p1 > tpr p2) then p1 else p2

--4. Una función que reciba una lista de presidentes y devuelva el nombre
--   del presidente que menos tiempo gobernó.

name:: Presidente -> Nombre
name (n,((_,_,_),(_,_,_))) = n

type ListPres = [Presidente]
mengob:: ListPres -> Presidente
mengob [] = error "?"
mengob [x] = x
mengob (x:xs) = if(gob x (mengob xs) == x)then x else mengob xs

presidentetiempomen :: ListPres -> Nombre
presidentetiempomen xs = name (mengob xs)

--5. Una función que reciba una lista de presidentes y devuelva una lista
--   con los nombres de los presidentes que gobernaron antes del año 1990.

anio:: Presidente -> Anio
anio (_,((_,_,_),(a2,_,_))) = a2

listaPr:: ListPres -> ListPres
listaPr [] = []
listaPr (x:xs) = if(anio x < 1990) then x:listaPr xs else listaPr xs

--6. Una función que reciba una lista de presidentes y devuelva la 
--   cantidad de presidentes que gobernaron menos de 4 años.

prconmenos4:: ListPres -> ListPres
prconmenos4 [] = []
prconmenos4 (x:xs) = if(tpr x < 4) then x:prconmenos4 xs else prconmenos4 xs

--7. Una función que reciba una lista de presidentes y la ordene 
--   ascendentemente por la fecha en que fue presidente.

orde [] = []
orde (x:xs) =  aux xs x []
    where aux [] m r = m : orde r
          aux (x:xs) m r | x < m     = aux xs x (m:r)
                         | otherwise = aux xs m (x:r)
 

--III. Sea el siguiente tipo de datos:
data Empleado = Docente Nombree Horas SueldoHora Materia | Administrativo Nombree Salario Cargo
    deriving Show 
type Nombree=String
type Horas=Int -- total de horas por mes
type SueldoHora=Float -- sueldo por hora
type Salario=Float -- salario mensual de el empleado
type Materia= [String] -- materias que dicta el docente
type Cargo=String -- cargo del administrativo
--a) Definir una función que reciba dos empleados y devuelva verdad si
--   ambos tienen el mismo ingreso mensual pero uno es docente y el otro 
--   es administrativo.
compara:: SueldoHora -> Salario -> Bool
compara sue sal = sue == sal

salario:: Empleado -> SueldoHora
salario (Docente _ _ s _) = s
salario (Administrativo _ s _) = s

comparaSalario :: Empleado -> Empleado -> Bool
comparaSalario d a = compara (salario d) (salario a)

doc1 = Docente "Juan" 120 15000.00 []
adm1 = Administrativo "Marta" 15000.00 "encargada"
adm2 = Administrativo "Daniel" 4000.00 "cajero"

--b) Definir una función que reciba una lista de empleados y devuelva el 
--   nombre del docente que dicta mayor cantidad de materias.
type Empleados = [Empleado]

debnom:: Empleado -> Nombree
debnom (Docente n _ _ _) = n
debnom (Administrativo n _ _) = n

tamat:: Empleado -> Int
tamat (Docente _ _ _ xs)= length xs
tamat (Administrativo _ _ _) = error "?"

mayornm:: Empleados -> Nombree
mayornm [] = error "?"
mayornm [x] = debnom x
mayornm (x:y:xs) = if (tamat x) >= (tamat y) then debnom x else mayornm (y:xs)

emps = [(Docente "a" 12 1 ["","",""]),(Docente "b" 12 3 ["","","","",""]),(Docente "c" 12 4 ["","","","","",""])]
emp1 = Docente "l" 12 3 [""]

--c) Definir una función que reciba una lista de empleados y devuelva el
--   nombre del empleado que tiene mayor salario mensual.
listemp::Empleados -> Nombre
listemp [] = error "?"
listemp [x] = debnom x
listemp (x:y:xs) = if (salario x >= salario y) then listemp (x:xs) else listemp (y:xs)

--d) Definir una función que reciba un empleado (e) y una lista ordenada 
--   ascendentemente de acuerdo al ingreso mensual percibido (ls) y que 
--   inserte e en ls en la posición que le corresponde, de modo que la 
--   lista resultante siga ordenada.
insertem::Empleado -> Empleados -> Empleados
insertem e [] = [e]
insertem e (l:ls)= if (salario e >= salario l) then l:e:insertem e (ls) else l:insertem e ls

--e) Definir una función que reciba una lista de empleados y devuelva la
--   lista ordenada ascendentemente por ingresos mensuales del empelado.
