module CLI (main) where

import TinyApp.Interactive (Key(..), Event(..), ContinueExit(..), runInteractive')
import Wordle (Juego(..), iniciarJuego, enviarIntento, juegoTerminado, palabraSecreta, mensaje,jugar,cantidadIntentosDisponibles, mostrarGrilla)


    
main :: IO ()
main = do
  putStrLn "Bienvenido a Wordle!"
  putStrLn "Ingrese la palabra secreta:"
  palabra <- getLine
  valido <- palabraInLista palabra (getListaPalabras "diccionario.txt")
  if valido 
    then let juegoInicial = iniciarJuego palabra 5 -- Convierte la palabra a mayúsculas
      in loopJuego juegoInicial
    else do
      main





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
      if palabraAdivinada juego
        then putStrLn $ "¡Felicidades! Adivinaste la palabra: " ++ palabraSecreta juego
        else putStrLn $ "¡Se acabaron los intentos! La palabra era: " ++ palabraSecreta juego
  | otherwise = do
      mostrarGrilla juego
      putStrLn $ "Intentos restantes: " ++ show (cantidadIntentosDisponibles juego)
      putStrLn "Ingrese su intento:"
      intento <- getLine
      valido <- palabraInLista intento (getListaPalabras "diccionario.txt")
      if valido
        then do
          let intentoMayuscula =  intento
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
