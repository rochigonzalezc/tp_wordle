{-# LANGUAGE OverloadedRecordDot #-}

module Game where

import Core (Match, match)


data Juego = Juego {palabraSecreta :: String, intentosDisponibles :: Int, intentosTotales :: Int, intentos :: [(String, [(Char, Match)])], palabraAdivinada :: Bool, mensaje :: Maybe String}
  deriving (Show)

iniciarJuego :: String -> Int -> Juego
iniciarJuego palabra intentos = Juego {palabraSecreta = palabra, intentosDisponibles = intentos, intentosTotales = intentos, intentos = [], palabraAdivinada = False, mensaje = Nothing}

-- Enviar un intento y saber si el intento fue aceptado o no por alguna condición de error como longitud inválida o Char inválido.

enviarIntento :: Juego -> String -> Juego
enviarIntento juego intento
  | length intento /= length (palabraSecreta juego) =
      juego
        { mensaje = Just "Longitud invalida del intento."
        }
  | not $ all (`elem` ['A' .. 'Z']) intento =
      juego
        { mensaje = Just "Caracteres invalidos en el intento."
        }
  | otherwise =
      juego
        { intentosDisponibles = intentosDisponibles juego - 1,
          intentos = (intento, match (palabraSecreta juego) intento) : intentos juego,
          palabraAdivinada = palabraAdivinada juego || intento == palabraSecreta juego,
          mensaje = Nothing
        }

-- Obtener si un juego terminó o no y si se logró adivinar la palabra o si se llegó a la cantidad de intentos posibles.
juegoTerminado :: Juego -> Bool
juegoTerminado juego = intentosDisponibles juego == 0 || palabraAdivinada juego

-- Obtener la longitud de la palabra secreta, cantidad de intentos disponibles, cantidad de intentos totales posibles.
longitudPalabra :: Juego -> Int
longitudPalabra juego = length $ palabraSecreta juego

cantidadIntentosDisponibles :: Juego -> Int
cantidadIntentosDisponibles juego = intentosDisponibles juego

cantidadIntentosTotales :: Juego -> Int
cantidadIntentosTotales juego = intentosTotales juego
