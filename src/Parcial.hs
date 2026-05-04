module Parcial where
import Text.Show.Functions()

data Perro = UnPerro {
    raza::String,
    juguetesFavs::[String],
    tiempoEnGuarderia::Int, -- Poner tiempo en minutos
    energia::Int
} deriving(Show, Eq)

data Guarderia = UnaGuarderia{
     nombreGuarderia::String,
     rutina::Rutina
}deriving(Show, Eq)

type Rutina = [(ejercicio, tiempo)]
type Ejercicio = Perro->Perro
type Tiempo = Int

modificarEnergia::(Int->Int)->Ejercicio
modificarEnergia unaFuncion unPerro = unPerro {energia=max (unaFuncion(unaEnergia unPerro)) 0}

-- Ejercicios
jugar::Ejercicio
jugar unPerro = modificarEnergia (-10) unPerro

ladrar::Int->Ejercicio
ladrar ladridosEstablecidos unPerro = modificarEnergia (div ladridosEstablecidos 2) unPerro 

regalar::String->Ejercicio
regalar unRegalo unPerro = unRegalo : (juguetesFavs unPerro)
-- Nose si está bien usado la funcion ":"

diaDeSpa::Ejercicio
diaDeSpa unPerro
|(unTiempo unPerro) > 50 = (regalar "peine de goma" . modificarEnergia (const 100)) unPerro
|(raza unPerro) == esDeRazaExtravagante =  (regalar "peine de goma" . modificarEnergia (const 100)) unPerro
|otherwise unPerro

esDeRazaExtravagante::Perro->Bool
esDeRazaExtravagante unPerro = raza unPerro == "dalmata" || raza unPerro == "pomerania"

diaDeCampo::Ejercicio
diaDeCampo unPerro = tail (juguetesFavs unPerro)

-- Perros
zara::Perro
zara = unPerro {
    raza = "dalmata",
    juguetesFavs = ["pelota", "mantita"]
    tiempoEnGuarderia = 90 --Tiempo en minutos 
    energia = 80
}

-- Guarderias
guarderíaPdePerritos::Guarderia
guarderíaPdePerritos = UnaGuarderia {
    nombre = "PdePerritos"
    rutina = [(jugar, 30), (ladrar 18, 20), (regalar "pelota", 0), (diaDeSpa,120), (diaDeCampo, 720)]
}

-- PARTE B
puedeEstarEnGuarderia::Perro->Guarderia->String
puedeEstarEnGuarderia unPerro unaGuarderia --Le paso la guarderia, pq si añadimos guarderias podemos reutilizar el codigo 
| tiempoEnGuarderia unPerro > tiempoDeRutina unaGuarderia = "El perro puede estar en la guarderia" 
| otherwise = "El perro no puede estar en la guarderia"

listaDeDuraciones::Guarderia->[Int]
listaDeDuraciones unaGuarderia = map snd (rutina unaGuarderia)

tiempoDeRutina::Guarderia->Int
tiempoDeRutina unaGuarderia = sum listaDeDuraciones
  
perroEsResponsable::Perro->String
perroEsResponsable unPerro 
|length (juguetesFavs unPerro) > 4 = "El perro es responsable" --No pongo >= pq si tiene 4 juguetes, y pierde 1 en el dia de campo no tendría MAS DE 3 como pide la consigna
|otherwise = "El perro NO es responsable"
