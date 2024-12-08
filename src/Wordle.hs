-- src/Wordle.hs
{-# LANGUAGE OverloadedRecordDot #-}

module Wordle where

import Control.Monad (forM_)
import Core (Match (..), match)
import Data.Char (toUpper)
import Data.List (nub)
import System.Console.ANSI
  ( Color (..),
    ColorIntensity (..),
    ConsoleLayer (..),
    SGR (..),
    clearScreen,
    hideCursor,
    setCursorPosition,
    setSGR,
    showCursor,
  )
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, readFile, stdin)


data Juego = Juego
  { palabraSecreta :: String,
    intentosDisponibles :: Int,
    intentosTotales :: Int,
    intentos :: [(String, [(Char, Match)])],
    palabraAdivinada :: Bool,
    mensaje :: Maybe String,
    letrasUsadas :: [Char],
    currentAttempt :: String -- Intento actual
  }
  deriving (Show)

iniciarJuego :: String -> Int -> Juego
iniciarJuego palabra intentos =
  Juego
    { palabraSecreta = map toUpper palabra,
      intentosDisponibles = intentos,
      intentosTotales = intentos,
      intentos = [],
      palabraAdivinada = False,
      mensaje = Nothing,
      letrasUsadas = [],
      currentAttempt = ""
    }

-- Funciones auxiliares

estaEnPalabra :: Char -> String -> Bool
estaEnPalabra letra "" = False
estaEnPalabra letra (p : palabra) = if letra == p then True else estaEnPalabra letra palabra

devolverLetrasIncorrectas :: String -> String -> [Char]
devolverLetrasIncorrectas "" _ = []
devolverLetrasIncorrectas (x : xs) palabraSecreta =
  if not (estaEnPalabra x palabraSecreta)
    then x : devolverLetrasIncorrectas xs palabraSecreta
    else devolverLetrasIncorrectas xs palabraSecreta

dejarSinRepetir :: [Char] -> [Char]
dejarSinRepetir [] = []
dejarSinRepetir (x : xs) = if x `elem` xs then dejarSinRepetir xs else x : dejarSinRepetir xs

-- Función para leer el diccionario de palabras
getListaPalabras :: FilePath -> IO [String]
getListaPalabras path = do
  content <- readFile path
  return (lines content)

-- Función para enviar un intento
enviarIntento :: Juego -> String -> IO Juego
enviarIntento juego intento = do
  palabras <- getListaPalabras "diccionario.txt"
  let palabraMayuscula = map toUpper intento
  if palabraMayuscula `elem` map (map toUpper) palabras
    then
      if length palabraMayuscula /= longitudPalabra juego
        then
          return juego {mensaje = Just "Longitud inválida del intento."}
        else
          if not (all (`elem` ['A' .. 'Z']) palabraMayuscula)
            then
              return juego {mensaje = Just "Caracteres inválidos en el intento."}
            else do
              let juegoActualizado =
                    juego
                      { intentosDisponibles = intentosDisponibles juego - 1,
                        intentos = (palabraMayuscula, match (palabraSecreta juego) palabraMayuscula) : intentos juego,
                        palabraAdivinada = palabraAdivinada juego || palabraMayuscula == palabraSecreta juego,
                        mensaje = Nothing,
                        letrasUsadas = dejarSinRepetir (devolverLetrasIncorrectas palabraMayuscula (palabraSecreta juego) ++ letrasUsadas juego),
                        currentAttempt = "" -- Resetear el intento actual después de enviar
                      }
              return juegoActualizado
    else
      return juego {mensaje = Just "La palabra no está en el diccionario."}

-- Función para verificar si el juego ha terminado
juegoTerminado :: Juego -> Bool
juegoTerminado juego = intentosDisponibles juego == 0 || palabraAdivinada juego

-- Funciones para obtener información del juego
longitudPalabra :: Juego -> Int
longitudPalabra juego = length $ palabraSecreta juego

cantidadIntentosDisponibles :: Juego -> Int
cantidadIntentosDisponibles juego = intentosDisponibles juego

cantidadIntentosTotales :: Juego -> Int
cantidadIntentosTotales juego = intentosTotales juego

-- ANSI color codes
ansiResetColor, ansiBgYellowColor, ansiBgGreenColor, ansiBgRedColor :: [SGR]
ansiResetColor = [Reset]
ansiBgYellowColor = [SetColor Background Vivid Yellow]
ansiBgGreenColor = [SetColor Background Vivid Green]
ansiBgRedColor = [SetColor Background Vivid Red]

-- Función para generar una línea de borde, e.g., +---+---+---+---+---+
lineaBorde :: Int -> String
lineaBorde n = "+" ++ concat (replicate n "---+")

-- Función para generar una línea de celdas con letras coloreadas
lineaCeldas :: String -> [(Char, Match)] -> IO ()
lineaCeldas palabra matches = do
  putStr "   |"
  forM_ (zip palabra matches) $ \(c, m) -> do
    setSGR $ case snd m of
      Correcto -> ansiBgGreenColor
      LugarIncorrecto -> ansiBgYellowColor
      NoPertenece -> ansiBgRedColor
    putStr $ " " ++ [c] ++ " |"
  setSGR [Reset]
  putStrLn ""

-- Función para generar una línea de celdas vacías
lineaCeldasVacias :: Int -> IO ()
lineaCeldasVacias n = do
  putStr "   |"
  forM_ [1 .. n] $ \_ -> putStr "   |"
  putStrLn ""

-- Función para mostrar el intento actual dentro de la grilla
displayCurrentAttempt :: String -> Int -> [Char] -> IO ()
displayCurrentAttempt attempt n letrasDescartadas = do
  putStr "   |"
  forM_ [1 .. n] $ \i -> do
    if i <= length attempt
      then do
        let char = attempt !! (i - 1)
        if char `elem` letrasDescartadas
          then setSGR [SetColor Background Vivid Red] 
          else setSGR [SetColor Background Vivid White] 
        putStr $ " " ++ [char] ++ " |"
      else do
        setSGR [Reset]
        putStr "   |"
  setSGR [Reset]
  putStrLn "   "

-- Función para mostrar la grilla completa incluyendo el intento actual
mostrarGrilla :: Juego -> IO ()
mostrarGrilla juego = do
  clearScreen 
  setCursorPosition 0 0
  hideCursor 
  let n = longitudPalabra juego
      totalLineas = intentosTotales juego
      intentosList = reverse $ intentos juego
    
      intentosCompletos = intentosList ++ replicate (totalLineas - length intentosList) ("", replicate n (' ', NoPertenece))

  putStrLn $ "   " ++ lineaBorde n

  -- Iterar sobre cada intento y mostrarlo
  forM_ intentosCompletos $ \(intento, matches) -> do
    if null intento
      then lineaCeldasVacias n
      else lineaCeldas intento matches
    putStrLn $ "   " ++ lineaBorde n

  -- Mostrar el intento actual en la siguiente fila de la grilla
  putStrLn "   Intento: "
  displayCurrentAttempt (currentAttempt juego) n (letrasUsadas juego)

  showCursor 
  
  -- Mostrar letras usadas que no están en la palabra secreta
  putStrLn "\nLetras descartadas: "
  putStrLn $ unwords $ map (:[]) $ letrasUsadas juego