module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

--1

data Hechicero = Hechicero {
    antiguedad :: Number,
    clan :: String,
    grado :: Number,
    cargo :: String
} deriving Show



nobara :: Hechicero
nobara = Hechicero {
    antiguedad = 1,
    clan = "Kugisaki",
    grado = 3,
    cargo = "Estudiante"
}

satoru :: Hechicero
satoru = Hechicero {
    antiguedad = 15,
    clan = "Gojo",
    grado = 0,
    cargo = "Maestro"
}

-- maki Hechicero 4 "Zenin" 4 "Estudiante" TAMBIEN SE PUEDE MODELAR ASÍ
maki :: Hechicero
maki = Hechicero {
    antiguedad = 4,
    clan = "Zenin",
    grado = 4,
    cargo = "Estudiante"
}

yuji :: Hechicero
yuji = Hechicero {
    antiguedad = 0,
    clan = "Itadori",
    grado = 1,
    cargo = "Estudiante"
}


--2

type Equipo = [Hechicero]

equipo1 :: Equipo
equipo1 = [yuji, satoru, nobara, maki]

equipo2 :: Equipo
equipo2 = [satoru]

tieneExperiencia :: Hechicero -> Bool
tieneExperiencia unHechicero = antiguedad unHechicero > 1

--3
estaPreparado :: Equipo -> Bool
estaPreparado unEquipo = length unEquipo > 3

--4

bajarRango :: Hechicero -> Hechicero
bajarRango unHechicero =  unHechicero {grado = grado unHechicero - 1}

esEspecial :: Hechicero ->  Bool
esEspecial unHechicero = grado unHechicero == 0

subirDeRango :: Hechicero -> Hechicero
subirDeRango unHechicero 
    | esEspecial unHechicero = unHechicero
    | otherwise = bajarRango unHechicero

--5

esPrestijioso :: Hechicero -> Bool
esPrestijioso unHechicero = elem (clan unHechicero) clanesPrestigiosos

clanesPrestigiosos = [ "Kamo", "Zenin", "Gojo"]

--                  SEGUNDA PARTE
--6

equipoInvencible :: Equipo -> Bool
equipoInvencible unEquipo = any esEspecial unEquipo

--7

esFavorito :: Equipo -> Bool
esFavorito unEquipo = all esPrestijioso unEquipo

--8

losExpertos :: Equipo -> Equipo
losExpertos unEquipo = filter tieneExperiencia unEquipo

--9
--a

--haceFrenteACualquiera equi

--b

powerUp :: Equipo ->Equipo
powerUp unEquipo = map subirDeRango unEquipo
