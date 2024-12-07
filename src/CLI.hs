module CLI (main) where

import Data.Char (toUpper)
import System.Environment (getArgs)
import System.Random.Stateful (globalStdGen, uniformRM)
import TinyApp.Interactive (ContinueExit (..), Event (..), Key (..), runInteractive')
import Wordle (Juego (..), cantidadIntentosDisponibles, enviarIntento, iniciarJuego, juegoTerminado, mensaje, mostrarGrilla, palabraSecreta, quitarPrefijo, stringListToString)

main :: IO ()
main = do
  args <- getArgs
  putStrLn (stringListToString args)
  putStrLn "Bienvenido a Wordle!"

  -- Obtener la lista de palabras desde el diccionario
  palabras <- getListaPalabras "diccionario.txt"

  position <- uniformRM (0, length palabras - 1) globalStdGen

  if (stringListToString args) == "--random"
    then do
      palabra <- return $ palabras !! position
      -- putStrLn "Palabra secreta: "
      let juegoInicial = iniciarJuego palabra 5 -- Convierte la palabra a mayúsculas
      loopJuego juegoInicial
    else do
      let palabra = map toUpper (quitarPrefijo "--palabra" (stringListToString args))
      let juegoInicial = iniciarJuego palabra 5 -- Convierte la palabra a mayúsculas
      loopJuego juegoInicial

-- funcion que lee diccionario.txt y me devuelva una lista
-- leerArchivo :: String -> [IO String]
-- leerArchivo archivo = do
--   contenido <- readFile archivo
--   let lineas = lines contenido
--   return lineas

getListaPalabras :: FilePath -> IO [String]
getListaPalabras archivo = do
  diccionario <- readFile archivo
  let palabras = lines diccionario
  return palabras

palabraInLista :: String -> IO [String] -> IO Bool
palabraInLista palabra lista = do
  lista' <- lista
  if palabra `elem` lista'
    then putStrLn "La palabra ingresada se encuentra en el diccionario."
    else putStrLn "La palabra ingresada no se encuentra en el diccionario."
  return (palabra `elem` lista')

-- Bucle principal del juego
loopJuego :: Juego -> IO ()
loopJuego juego
  | juegoTerminado juego = do
      mostrarGrilla juego
      if palabraAdivinada juego
        then putStrLn $ "¡Felicidades! Adivinaste la palabra: " ++ palabraSecreta juego
        else putStrLn $ "¡Se acabaron los intentos! La palabra era: " ++ palabraSecreta juego
  | otherwise = do
      mostrarGrilla juego
      putStrLn $ "Intentos restantes: " ++ show (cantidadIntentosDisponibles juego)
      putStrLn $ "Letras usadas: " ++ letrasUsadas juego
      putStrLn "Ingrese su intento:"
      intento <- getLine
      valido <- palabraInLista intento (getListaPalabras "diccionario.txt")
      if valido
        then do
          let intentoMayuscula = intento
          let juegoActualizado = enviarIntento juego intentoMayuscula
          case mensaje juegoActualizado of
            Just msg -> do
              putStrLn msg -- Mensaje de error
              loopJuego juegoActualizado
            Nothing -> loopJuego juegoActualizado
        else do
          loopJuego juego

-- let intentoMayuscula =  intento
-- let juegoActualizado = enviarIntento juego intentoMayuscula
-- case mensaje juegoActualizado of
--   Just msg -> do
--     putStrLn msg -- Mensaje de error
--     loopJuego juegoActualizado
--   Nothing -> loopJuego juegoActualizado
