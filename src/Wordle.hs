{-# LANGUAGE OverloadedRecordDot #-}
module Wordle where
import Core (Match(..), match)

data Juego = Juego {palabraSecreta :: String, intentosDisponibles :: Int, intentosTotales :: Int, intentos :: [(String, [(Char, Match)])], palabraAdivinada :: Bool, mensaje :: Maybe String}
  deriving (Show)

iniciarJuego :: String -> Int -> Juego
iniciarJuego palabra intentos = Juego {palabraSecreta = palabra, intentosDisponibles = intentos, intentosTotales = intentos, intentos = [], palabraAdivinada = False, mensaje = Nothing}

-- Enviar un intento y saber si el intento fue aceptado o no por alguna condición de error como longitud inválida o Char inválido.

enviarIntento :: Juego -> String -> Juego
enviarIntento juego intento
  | length intento /= length (palabraSecreta juego) =
      juego {mensaje = Just "Longitud invalida del intento."}
  | not $ all (`elem` ['A' .. 'Z']) intento =
      juego {mensaje = Just "Caracteres invalidos en el intento."}
  | otherwise =
      juego { intentosDisponibles = intentosDisponibles juego - 1,
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

-- ANSI color codes
ansiResetColor, ansiBgYellowColor, ansiBgGreenColor, ansiBgRedColor :: String
ansiResetColor = "\ESC[39m\ESC[49m"
ansiBgYellowColor = "\ESC[103m"
ansiBgGreenColor = "\ESC[42m"
ansiBgRedColor = "\ESC[41m"

-- Función para mostrar la grilla actual con colores
mostrarGrilla :: Juego -> IO ()
mostrarGrilla juego = do
  putStrLn "\nGrilla:"
  mapM_ (putStrLn . formatoIntento) (reverse $ intentos juego)
  putStrLn $ replicate (longitudPalabra juego * 4 - 1) '-'
  where
    formatoIntento (intento, matches) =
      concatMap (\(c, m) -> formatoChar c m ++ " ") matches ++ ansiResetColor
    formatoChar c Correcto = ansiBgGreenColor ++ " " ++ [c] ++ " "
    formatoChar c LugarIncorrecto = ansiBgYellowColor ++ " " ++ [c] ++ " "
    formatoChar c NoPertenece = ansiBgRedColor ++ " " ++ [c] ++ " "
-- Bucle principal del juego
jugar :: Juego -> IO ()
jugar juego
  | juegoTerminado juego = do
      if palabraAdivinada juego
        then putStrLn $ "¡Felicidades! Adivinaste la palabra: " ++ palabraSecreta juego
        else putStrLn $ "¡Se acabaron los intentos! La palabra era: " ++ palabraSecreta juego
  | otherwise = do
      putStrLn $ "\nIntentos disponibles: " ++ show (cantidadIntentosDisponibles juego)
      mostrarGrilla juego
      putStrLn "Ingresa tu intento:"
      intento <- getLine
      let intentoMayuscula =  intento
      let juegoActualizado = enviarIntento juego intentoMayuscula
      case mensaje juegoActualizado of
        Just msg -> do
          putStrLn msg
          jugar juegoActualizado
        Nothing -> jugar juegoActualizado
