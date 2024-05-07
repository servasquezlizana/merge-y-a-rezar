module Library where
import PdePreludat

data Ciudad = Ciudad {
    nombre :: String,
    añoFundacion :: Number,
    atracciones :: [String],
    costoDeVida :: Number
} deriving (Show)

-- Punto 1: Valor de una ciudad (común para todos los integrantes)

valorCiudad :: Ciudad -> Number
valorCiudad (Ciudad _ año atracciones costo)    | año < 1800 = 5 * (1800 - año)
                                                | length atracciones == 0 = costo * 2
                                                | otherwise = costo * 3

-- Punto 2: Características de las ciudades

-- Integrante 1: Alguna atracción copada
isVowel :: Char -> Bool
isVowel character = character `elem` "aeiouAEIOU"

-- Integrante 2: Ciudad Sobria
sobriaCiudad :: Ciudad -> Number -> Bool
sobriaCiudad (Ciudad _ _ latracciones _) valorAjustable | length latracciones == 0 = False
                                                        | filter ((> valorAjustable).length) latracciones == latracciones = True
                                                        | otherwise = False


--Estimo que este punto se debe hacer con orden superior. El miercoles que viene lo veremos

--Punto 3: Eventos

--Sumar una nueva atracción (todos los integrantes)

sumarAtraccion :: Ciudad -> [String] -> Ciudad
sumarAtraccion (Ciudad nombre año atracciones costo) nuevaAtraccion = Ciudad nombre año (nuevaAtraccion ++ atracciones) (costo*1.2)

-- Integrante 2: Remodelacion
remodelacion :: Ciudad -> String -> Ciudad
remodelacion (Ciudad nombre año atracciones costo) city = Ciudad city año atracciones (costo * 1.5)

