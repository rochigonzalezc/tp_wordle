{-# LANGUAGE OverloadedRecordDot #-}
module Wordle where
import Core (Match(..), match)

data Juego = Juego {palabraSecreta :: String, intentosDisponibles :: Int, intentosTotales :: Int, intentos :: [(String, [(Char, Match)])], palabraAdivinada :: Bool, mensaje :: Maybe String, letrasUsadas :: [Char]}
  deriving (Show)

iniciarJuego :: String -> Int -> Juego
iniciarJuego palabra intentos = Juego {palabraSecreta = palabra, intentosDisponibles = intentos, intentosTotales = intentos, intentos = [], palabraAdivinada = False, mensaje = Nothing, letrasUsadas = []}

-- Enviar un intento y saber si el intento fue aceptado o no por alguna condición de error como longitud inválida o Char inválido.

estaEnPalabra :: Char -> String -> Bool
estaEnPalabra letra "" = False
estaEnPalabra letra (p:palabra)= if letra == p then True else estaEnPalabra letra palabra

devolverLetrasIncorrectas :: String -> String -> [Char]
devolverLetrasIncorrectas "" _ = []
devolverLetrasIncorrectas (x:xs) palabraSecreta = if not (estaEnPalabra x palabraSecreta) then x : devolverLetrasIncorrectas xs palabraSecreta else devolverLetrasIncorrectas xs palabraSecreta

dejarSinRepetir :: [Char] -> [Char]
dejarSinRepetir [] = []
dejarSinRepetir (x:xs) = if x `elem` xs then dejarSinRepetir xs else x : dejarSinRepetir xs


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
              mensaje = Nothing, letrasUsadas = dejarSinRepetir (devolverLetrasIncorrectas intento (palabraSecreta juego) ++ letrasUsadas juego)
              
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


stringListToString ::[String] -> String
stringListToString [] = ""
stringListToString (x:xs) = x ++ stringListToString xs


quitarPrefijo :: String -> String -> String
quitarPrefijo palabra1 palabra2 =
  case stripPrefix palabra1 palabra2 of
    Just resto -> resto -- Si palabra1 es un prefijo, devolvemos el resto
    Nothing    -> palabra2 -- Si no, devolvemos palabra2 sin cambios

-- Función que elimina un prefijo de una cadena si existe
stripPrefix :: String -> String -> Maybe String
stripPrefix [] str = Just str          -- Si el prefijo está vacío, la cadena no cambia
stripPrefix _ [] = Nothing             -- Si la cadena está vacía pero el prefijo no, no hay coincidencia
stripPrefix (x:xs) (y:ys)
  | x == y    = stripPrefix xs ys     -- Si los caracteres coinciden, continúa con el resto
  | otherwise = Nothing                -- Si no coinciden, no es un prefijo


esPalabra :: String -> String -> Bool
esPalabra _ [] = True  -- Si la palabra está vacía o el texto se ha agotado, no hay coincidencia.
esPalabra [] _ = False   -- Si la palabra está vacía, siempre se considera que está en cualquier texto (o es trivialmente aceptada).
esPalabra (p:ps) (t:ts)
  | (p == t) = esPalabra ps ts  -- Si los primeros caracteres coinciden, continuamos comparando el resto de la palabra y el texto.
  | otherwise = esPalabra (p:ps) ts  -- Si no coinciden, seguimos buscando la palabra en el resto del texto.
