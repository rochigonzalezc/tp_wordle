-- src/Main.hs
module CLI where

import Data.Char (toUpper)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)
import System.Random.Stateful (globalStdGen, uniformRM)
import Wordle

-- Función para convertir una lista de Strings a un solo String
stringListToString :: [String] -> String
stringListToString [] = ""
stringListToString (x : xs) = x ++ stringListToString xs

-- Función para quitar un prefijo de una cadena si existe
quitarPrefijo :: String -> String
quitarPrefijo "" = ""
quitarPrefijo palabra =
  if palabra !! 0 == '-'
    then quitarPrefijo (tail palabra)
    else
      palabra

-- Función que elimina un prefijo de una cadena si existe
stripPrefix :: String -> String -> Maybe String
stripPrefix [] str = Just str -- Si el prefijo está vacío, la cadena no cambia
stripPrefix _ [] = Nothing -- Si la cadena está vacía pero el prefijo no, no hay coincidencia
stripPrefix (x : xs) (y : ys)
  | x == y = stripPrefix xs ys -- Si los caracteres coinciden, continúa con el resto
  | otherwise = Nothing -- Si no coinciden, no es un prefijo

-- Función para manejar la entrada del usuario
handleInput :: Juego -> IO Juego
handleInput juego = do
  c <- readCharRaw
  let upperC = toUpper c
  if upperC == '\ESC'
    then
      exitSuccess
    else
      if upperC == '\DEL' || upperC == '\b'
        then
          let current = currentAttempt juego
              newAttempt = if null current then current else init current
           in return juego {currentAttempt = newAttempt}
        else
          if upperC == '\n' || upperC == '\r'
            then
              if length (currentAttempt juego) == longitudPalabra juego
                then do
                  juegoActualizado <- enviarIntento juego (currentAttempt juego)
                  return juegoActualizado
                else do
                  -- Mostrar mensaje de error
                  putStrLn "\nCompleta la palabra antes de enviar."
                  return juego
            else
              if length (currentAttempt juego) < longitudPalabra juego && upperC `elem` ['A' .. 'Z']
                then
                  let newAttempt = currentAttempt juego ++ [upperC]
                   in return juego {currentAttempt = newAttempt}
                else
                  return juego -- Ignorar otros caracteres

-- Función para leer un carácter sin esperar enter
readCharRaw :: IO Char
readCharRaw = getChar

-- Bucle principal del juego
jugar :: Juego -> IO ()
jugar juego
  | juegoTerminado juego = do
      mostrarGrilla juego
      if palabraAdivinada juego
        then putStrLn $ "¡Felicidades! Adivinaste la palabra: " ++ palabraSecreta juego
        else putStrLn $ "¡Se acabaron los intentos! La palabra era: " ++ palabraSecreta juego
  | otherwise = do
      mostrarGrilla juego
      -- Manejar entradas en tiempo real
      juegoActualizado <- handleInput juego
      jugar juegoActualizado

main :: IO ()
main = do
  -- Configurar el terminal
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  args <- getArgs
  putStrLn "Bienvenido a Wordle!"

  -- Obtener la lista de palabras desde el diccionario
  palabras <- getListaPalabras "diccionario.txt"

  -- Determinar si se usa la opción --random o se proporciona una palabra específica
  juegoInicial <-
    if "--random" `elem` args
      then do
        -- Generar un índice aleatorio para seleccionar una palabra
        position <- uniformRM (0, length palabras - 1) globalStdGen
        let palabra = palabras !! position
        return $ iniciarJuego palabra 6 -- 6 intentos
      else do
        putStrLn (stringListToString args)
        -- Extraer la palabra secreta de los argumentos
        let palabra = quitarPrefijo (stringListToString args)
        putStrLn palabra
        return $ iniciarJuego palabra 6 -- 6 intentos
  jugar juegoInicial
