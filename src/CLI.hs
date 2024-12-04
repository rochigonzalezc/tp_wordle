module CLI (main) where

import TinyApp.Interactive (Key(..), Event(..), ContinueExit(..), runInteractive')
import Wordle (Juego(..), iniciarJuego, enviarIntento, juegoTerminado, palabraSecreta, mensaje,jugar,cantidadIntentosDisponibles, mostrarGrilla)


    
main :: IO ()
main = do
  putStrLn "Bienvenido a Wordle!"
  putStrLn "Ingrese la palabra secreta:"
  palabra <- getLine
  let juegoInicial = iniciarJuego palabra 5 -- Convierte la palabra a mayúsculas
  loopJuego juegoInicial

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
      let intentoMayuscula =  intento
      let juegoActualizado = enviarIntento juego intentoMayuscula
      case mensaje juegoActualizado of
        Just msg -> do
          putStrLn msg -- Mensaje de error
          loopJuego juegoActualizado
        Nothing -> loopJuego juegoActualizado
