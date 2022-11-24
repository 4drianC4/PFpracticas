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


--5. Una función que reciba una lista de presidentes y devuelva una lista
--   con los nombres de los presidentes que gobernaron antes del año 1990.
--6. Una función que reciba una lista de presidentes y devuelva la 
--   cantidad de presidentes que gobernaron menos de 4 años.
--7. Una función que reciba una lista de presidentes y la ordene 
--   ascendentemente por la fecha en que fue presidente.

--III. Sea el siguiente tipo de datos:
--data Empleado = Docente Nombre Horas SueldoHra Materias
-- | Administrativo Nombre Salario Cargo
--type Nombre=String
--type Horas=Int -- total de horas por mes
--type SueldoHora=Float -- sueldo por hora
--type Salario=Float -- salario mensual de el empleado
--type Materia= [String] -- materias que dicta el docente
--type Cargo=String -- cargo del administrativo
--a) Definir una función que reciba dos empleados y devuelva verdad si
--   ambos tienen el mismo ingreso mensual pero uno es docente y el otro 
--   es administrativo.
--b) Definir una función que reciba una lista de empleados y devuelva el 
--   nombre del docente que dicta mayor cantidad de materias.
--c) Definir una función que reciba una lista de empleados y devuelva el
--   nombre del empleado que tiene mayor salario mensual.
--d) Definir una función que reciba un empleado (e) y una lista ordenada 
--   ascendentemente de acuerdo al ingreso mensual percibido (ls) y que 
--   inserte e en ls en la posición que le corresponde, de modo que la 
--   lista resultante siga ordenada.
--e) Definir una función que reciba una lista de empleados y devuelva la
--   lista ordenada ascendentemente por ingresos mensuales del empelado.